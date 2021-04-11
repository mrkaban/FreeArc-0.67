// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Single block compressor for slow methods (-m3..-m5) using matching with fixed-size blocks ********************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool record_match (bool ROUND_MATCHES, unsigned L, unsigned MIN_MATCH, unsigned BASE_LEN, Offset block_start, HashTable &h, char *buf, int block_size, STAT *&stat, unsigned last_match_end, unsigned &match_end, unsigned &literal_bytes, unsigned i, Chunk k)
{
  unsigned add_len, match_len = h.match_len (k, &buf[last_match_end], &buf[i], buf+block_size, block_start, buf, &add_len);

  if (match_len >= MIN_MATCH)                                                                          // If match is large enough, then ...
  {
    unsigned match_start = i-add_len;
    if (ROUND_MATCHES)
      match_len = match_len/L*L;

    Offset match_offset = block_start+i-Offset(k)*L;
    if (match_offset < pc.max_offset)  pc.record_match++, pc.total_match_len += match_len;
    ENCODE_LZ_MATCH(stat, ROUND_MATCHES, BASE_LEN,  match_start-last_match_end, match_offset, match_len);     // ... save triple (literal_len, match_offset, match_len)
    match_end = match_start+match_len;
    literal_bytes -= match_len;
    return true;
  }
  return false;
}

#define prefetch_and_store_match(i,hash,hashes_last)                                                                                                  \
{                                                                                                                                                     \
  if ((i)>=last_match_end  && (i)<match_start) {                                                                                                      \
    h.prefetch_match(hash);                                                                                                                           \
    *(hashes_last)++ = (hash);                                                                                                                        \
    *(hashes_last)++ = BigHash(i);                                                                                                                    \
  }                                                                                                                                                   \
}                                                                                                                                                     \

#define check_match(i,hash,match_found)                                                                                                               \
{                                                                                                                                                     \
  /*if ((i) >= last_match_end) */{                                                                                                                    \
    Chunk k = h.find_match(&buf[i], i, block_size, hash, block_start+i);     /* Number of previous chunk with the same contents as current one  */    \
    if (k != NOT_FOUND) {                                                    /* If match was found - record it                                  */    \
      unsigned match_end;                                                                                                                             \
      if (record_match(ROUND_MATCHES, L, MIN_MATCH, BASE_LEN, block_start, h, buf, block_size, stat, last_match_end, match_end, literal_bytes, i, k)) { \
        last_match_end = match_end;                                                                                                                   \
        goto match_found;                                                                                                                             \
      }                                                                                                                                               \
    }                                                                                                                                                 \
  }                                                                                                                                                   \
}


// Compress buf[] and return compressed data in outbuf[] and statbuf[]
template <unsigned ACCELERATOR>
void compress (bool ROUND_MATCHES, unsigned L, unsigned MIN_MATCH, unsigned BASE_LEN, Offset block_start, HashTable &h, char *buf, int block_size, unsigned &literal_bytes, STAT *in_statbuf, STAT *statbuf, STAT *&stat)
{
  stat = statbuf;                                                               // where to save next found match info
  NUMBER last_match_end = 0;                                                    // points to the end of last match written, we shouldn't start new match before it
  literal_bytes = block_size;                                                   // number of literal bytes = block size minus sum of match lengths
  const NUMBER CYCLES = (ACCELERATOR? ACCELERATOR:1),  OFFSET = CYCLES-1;

  // Decode first input match and save its params
  STAT *instat = in_statbuf;                                                                            // pointer to input matches
  DECODE_LZ_MATCH(instat, false, ROUND_MATCHES, BASE_LEN, block_start,  lit_len, LZ_MATCH, lz_match);
  unsigned match_start = lz_match.dest-block_start,  match_len = lz_match.len;
  Offset match_offset = lz_match.dest-lz_match.src;

  if (2*L <= block_size)
  {
    PolynomialRollingHash<BigHash>  hash1 (L-OFFSET, PRIME1);
    PolynomialRollingHash<BigHash>  hash2 (L,        PRIME1);

    // PRIME1 powers required to calculate hash2 from hash1 by adding a few missing bytes right *before* the hash1 window start
    BigHash PRIME1_powers[CYCLES];  PRIME1_powers[CYCLES-1]  =  power (BigHash(PRIME1), L-CYCLES);
    for (NUMBER j=CYCLES-1; j>0; j--)
      PRIME1_powers[j-1] = PRIME1_powers[j]*PRIME1;

    // SPECIAL HANDLING FOR FIRST L BYTES OF THE BLOCK
    for (NUMBER i=0; i<1; i++)
    {
      hash1.moveto(buf);                                                        // Hash of buf[0]..buf[L-OFFSET-1]
      hash2.value = hash1;
      for (NUMBER j=0; true; j++)
      {
        h.mark_match_possibility<ACCELERATOR> (hash1);                          // Mark bitarr for the last OFFSET bytes of chunk
        if (j==OFFSET) break;
        hash1.update (buf[j], buf[L-OFFSET+j]);

        hash2.update (0, buf[L-OFFSET+j]);                                      // Calc hash2(buf[0]..buf[L-1]) from the hash1(buf[0]..buf[L-OFFSET-1]):
      }                                                                         //   just add OFFSET next bytes while "removing" zero bytes

      check_match (i, hash2, match_found1);
      match_found1: ;
      h.add_hash (&buf[i], i, block_size, (block_start+i)/L, hash2, block_start+i);
    }

    // MAIN CYCLE, PROCESSING L-BYTE BLOCK AT EVERY STEP
    for (NUMBER i=0; i<=block_size-2*L; )
    {
      BigHash saved_hash1=0;

      // SUB-MAIN CYCLE, PROCESSING UP TO `LOOKAHEAD` BYTES AT EVERY STEP
      for (NUMBER next_chunk = i+L;  likely(i<next_chunk);  )
      {
        // PROCESS NEXT INPUT MATCH ONCE WE'VE REACHED ITS START
        if (i >= match_start)
        {
          // Encode input match if it doesn't overlap with our own one
          if (match_start >= last_match_end)
          {
            unsigned literal_len = match_start-last_match_end;
            ENCODE_LZ_MATCH(stat, ROUND_MATCHES, BASE_LEN,  literal_len,match_offset,match_len);
            last_match_end = match_start+match_len;
            literal_bytes -= match_len;
          }

          // Decode next input match and save its params
          DECODE_LZ_MATCH(instat, false, ROUND_MATCHES, BASE_LEN, block_start+match_start+match_len,  lit_len, LZ_MATCH, lz_match);
          match_start=lz_match.dest-block_start;  match_len=lz_match.len;  match_offset=lz_match.dest-lz_match.src;
        }

        // UPDATE hash1 UNTIL last_match_end, ROUNDED DOWN TO X-BYTES BOUNDARY
        const NUMBER X = mymax (CYCLES,4);
        NUMBER  next_i = mymin (next_chunk-CYCLES, (last_match_end>0?last_match_end-1:0));
        if (next_i >= i+L/2) {                                                  // Quick path for large updates
          i = next_i & ~(X-1);
          hash1.moveto(buf+i+OFFSET);                                           // Hash of buf[i+OFFSET]..buf[i+L-1]
        } else {
          for ( ; likely(i+X<=next_i); i+=X)
            hash1.update<X> (buf+i+OFFSET);
        }

        const int LOOKAHEAD = (ACCELERATOR? 256:128);  // 2*50 ns (TLB+Data) === LOOKAHEAD*hash1.update(1ns)  ==>  LOOKAHEAD ~= 100 bytes per cycle
        NUMBER last_i=mymin(next_chunk,i+LOOKAHEAD);  BigHash hashes1_buf[LOOKAHEAD];  BigHash hashes2_buf[LOOKAHEAD*2];

        // PREFETCH bitarr AND UPDATE hash1
        if (ACCELERATOR)
        {
          BigHash *hashes1 = hashes1_buf;
          for (NUMBER i0=i;  likely(i0<last_i);  i0 += CYCLES)                  // MAIN CYCLE, processing every input byte, except for (most part of) match contents
          {
            hash1.update (buf[i0+OFFSET], buf[i0+L]);
            h.prefetch_check_match_possibility<ACCELERATOR> (hash1);
            *hashes1++ = hash1;
            hash1.update<CYCLES-1> (buf+i0+1+OFFSET);
          }
          saved_hash1 = hashes1[-1];
        }

        // CHECK bitarr AND CONDITIONALLY PREFETCH chunkarr
        BigHash *hashes2_last=hashes2_buf;
        for (BigHash *hashes1=hashes1_buf;  likely(i<last_i);  )
        {
          if (ACCELERATOR)
          {
            BigHash hsh1 = *hashes1++;
            if (unlikely (h.check_match_possibility<ACCELERATOR> (hsh1) ))
            {
              i++;  hash2.value = hsh1;                                         // Calc hash2(buf[i]..buf[i+L-1]) from the hash1(buf[i+OFFSET]..buf[i+L-1])
              for (NUMBER j=0; j<CYCLES-1; j++)
                hash2.value  +=  BYTE(buf[i+j]) * PRIME1_powers[j];

              prefetch_and_store_match (i, hash2, hashes2_last);
              for (NUMBER j=0; j<CYCLES-1; i++, j++)
              {
                hash2.update(buf[i], buf[i+L]);
                prefetch_and_store_match (i+1, hash2, hashes2_last);
              }
            }
            else
            {
              i += CYCLES;
            }
          }
          else
          {
            for (NUMBER j=0; j<X; i++, j++)                                     // Code for ACCELERATOR==0
            {
              hash1.update(buf[i], buf[i+L]);
              prefetch_and_store_match (i+1, hash1, hashes2_last);
            }
          }
        }

        // CHECK chunkarr TRYING TO FIND MATCH
        for (BigHash *hashes2=hashes2_buf;  hashes2<hashes2_last;  )
        {
          BigHash hsh = *hashes2++;  NUMBER i = NUMBER(*hashes2++);
          check_match (i, hsh, match_found2);
        }
match_found2: ;
      }

      // SECOND SUB-MAIN CYCLE, REPROCESSING LAST `CYCLES` BYTES OF THE l-BYTE CHUNK
      if (ACCELERATOR)
      {
        hash2.value = hash1.value = saved_hash1;
        h.mark_match_possibility<ACCELERATOR> (hash1);                      // Mark bitarr for the last CYCLES bytes of chunk

        for (NUMBER j=1; j<CYCLES; j++)
        {
          hash1.update (buf[i-CYCLES+j+OFFSET], buf[i-CYCLES+j+L]);
          h.mark_match_possibility<ACCELERATOR> (hash1);                    // Mark bitarr for the last CYCLES bytes of chunk

          hash2.update (0, buf[i-CYCLES+j+L]);                              // Calc hash2(buf[i+CYCLES-1]..buf[i+L+CYCLES-2]) from the hash1(buf[i+CYCLES-1]..buf[i+L-1]):
        }                                                                   //   just add CYCLES-1 next bytes while "removing" zero bytes
      }
      h.add_hash (&buf[i], i, block_size, (block_start+i)/L, ACCELERATOR?hash2:hash1, block_start+i);
    }
  }
}
