extern "C" {
#include "7zAES/Aes.c"
#include "7zAES/CpuArch.c"

#include "C_Encryption.h"
#define LTC_NO_CIPHERS
#define   LTC_BLOWFISH
#define   LTC_RIJNDAEL
#define     ENCRYPT_ONLY
#define   LTC_TWOFISH
#define   LTC_SERPENT
#define LTC_NO_MODES
#define   LTC_CFB_MODE
#define   LTC_CTR_MODE
#define LTC_NO_HASHES
#define   LTC_SHA512
#define LTC_NO_MACS
#define   LTC_HMAC
#define LTC_NO_MATH
#define LTC_NO_PK
#define   LTC_MECC
#define LTC_NO_TEST
#include "ciphers/aes/aes.c"
#include "ciphers/blowfish.c"
#include "ciphers/twofish/twofish.c"
#include "ciphers/serpent.c"
#include "crypt/crypt_argchk.c"
#include "crypt/crypt_cipher_descriptor.c"
#include "crypt/crypt_cipher_is_valid.c"
#include "crypt/crypt_find_cipher.c"
#include "crypt/crypt_find_hash.c"
#include "crypt/crypt_find_prng.c"
#include "crypt/crypt_hash_descriptor.c"
#include "crypt/crypt_hash_is_valid.c"
#include "crypt/crypt_prng_descriptor.c"
#include "crypt/crypt_prng_is_valid.c"
#include "crypt/crypt_register_cipher.c"
#include "crypt/crypt_register_hash.c"
#include "crypt/crypt_register_prng.c"
#include "hashes/helper/hash_memory.c"
#include "hashes/sha2/sha512.c"
#include "mac/hmac/hmac_done.c"
#include "mac/hmac/hmac_init.c"
#include "mac/hmac/hmac_memory.c"
#include "mac/hmac/hmac_process.c"
#include "misc/error_to_string.c"
#include "misc/pkcs5/pkcs_5_2.c"
#include "misc/zeromem.c"
#include "modes/ctr/ctr_decrypt.c"
#include "modes/ctr/ctr_done.c"
#include "modes/ctr/ctr_encrypt.c"
#include "modes/ctr/ctr_start.c"
#include "modes/cfb/cfb_decrypt.c"
#include "modes/cfb/cfb_done.c"
#include "modes/cfb/cfb_encrypt.c"
#include "modes/cfb/cfb_start.c"
#include "prngs/fortuna.c"
}
#include "../MultiThreading.h"


/*-------------------------------------------------*/
/* Инициализация библиотеки шифрования LibTomCrypt */
/*-------------------------------------------------*/

// Зарегистировать все включённые в программу алгоритмы
int register_all()
{
    register_cipher (&aes_enc_desc);
    register_cipher (&blowfish_desc);
    register_cipher (&serpent_desc);
    register_cipher (&twofish_desc);
//  register_hash (&sha1_desc);
    register_hash (&sha512_desc);
#ifndef LTC_NO_TEST
    CHECK (FREEARC_ERRCODE_INTERNAL,  blowfish_test()==CRYPT_OK,  (s,"blowfish_test failed!"));
//  CHECK (FREEARC_ERRCODE_INTERNAL,  rijndael_test()==CRYPT_OK,  (s,"rijndael_test failed!"));
    CHECK (FREEARC_ERRCODE_INTERNAL,  serpent_test ()==CRYPT_OK,  (s,"serpent_test failed!"));
    CHECK (FREEARC_ERRCODE_INTERNAL,  twofish_test ()==CRYPT_OK,  (s,"twofish_test failed!"));
//  CHECK (FREEARC_ERRCODE_INTERNAL,  sha1_test    ()==CRYPT_OK,  (s,"sha1_test failed!"));
    CHECK (FREEARC_ERRCODE_INTERNAL,  sha512_test  ()==CRYPT_OK,  (s,"sha512_test failed!"));
//  CHECK (FREEARC_ERRCODE_INTERNAL,  hmac_test    ()==CRYPT_OK,  (s,"hmac_test failed!"));
//  CHECK (FREEARC_ERRCODE_INTERNAL,  ctr_test     ()==CRYPT_OK,  (s,"ctr_test failed!"));
//  CHECK (FREEARC_ERRCODE_INTERNAL,  cfb_test     ()==CRYPT_OK,  (s,"cfb_test failed!"));
#endif
    return 0;
}
int call_register_all = register_all();

// Размер буфера Fortuna PRNG
int fortuna_size (void)
{
    return sizeof(prng_state);
}


/*------------------------------------------------------*/
/* Обобщённый интерфейс к режимам шифрования (CFB,CTR)  */
/*------------------------------------------------------*/

#define roundup_ptr(p,n) ((p)+(n)-ptrdiff_t(p)%(n))

struct EncryptionMode
{
    enum {CTR,CFB}      mode;
    union
    {
        symmetric_CTR   ctr;
        symmetric_CFB   cfb;
        char            aesbuf [AES_NUM_IVMRK_WORDS*sizeof(UInt32) + AES_BLOCK_SIZE];    // buffer for g_AesCtr_Code
    };
    char               *aesctr;

    EncryptionMode (int _mode) {mode = (_mode==0? CTR:CFB);}

    char *name()
    {
        switch (mode) {
        case CTR: return "ctr";
        case CFB: return "cfb";
        default:  abort();
        }
    }

    int start (int cipher, BYTE *iv, BYTE *key, int keysize, int rounds)
    {
        // 7-zip code implements only AES-CTR with default number of rounds
        if (strequ(cipher_descriptor[cipher].name,"aes")  &&  mode==CTR  &&  rounds==0)
        {
            // Init the AES-CTR library
            static Mutex exclusive_access;
            {
                Lock _(exclusive_access);
                static volatile bool initialized = false;
                if (!initialized)
                {
                    initialized = true;
                    AesGenTables();
                    if (CPU_Is_Aes_Supported())
                    {
                        FARPROC Fast_AesCtr_Code = LoadFromDLL("Fast_AesCtr_Code");
                        if (Fast_AesCtr_Code)  g_AesCtr_Code = (AES_CODE_FUNC) Fast_AesCtr_Code;
                    }
                }
            }

            // aesctr[] should be aligned to 16-byte boundary
            aesctr = roundup_ptr (aesbuf, AES_BLOCK_SIZE);
            // Store in aesctr[] InitVector-1 (since 7-zip code does *pre*increment of counter)
            memcpy (aesctr, iv, AES_BLOCK_SIZE);
            for (int i=0; i<AES_BLOCK_SIZE; i++)
                if(aesctr[i]--) break;
            // Calculate and store in aesctr[] number of rounds and roundKeys
            Aes_SetKey_Enc ((UInt32*)(aesctr+AES_BLOCK_SIZE), key, keysize);
            return 0;
        }
        aesctr = NULL;

        switch (mode) {
        case CTR: return ctr_start (cipher, iv, key, keysize, rounds, CTR_COUNTER_LITTLE_ENDIAN, &ctr);
        case CFB: return cfb_start (cipher, iv, key, keysize, rounds, &cfb);
        default:  abort();
        }
    }

    int encrypt (BYTE *buf, int len)
    {
        if (aesctr)
        {
            g_AesCtr_Code ((UInt32*)aesctr, buf, len/AES_BLOCK_SIZE);
            return 0;
        }
        switch (mode) {
        case CTR: return ctr_encrypt (buf, buf, len, &ctr);
        case CFB: return cfb_encrypt (buf, buf, len, &cfb);
        default:  abort();
        }
    }

    int decrypt (BYTE *buf, int len)
    {
        if (aesctr)
        {
            g_AesCtr_Code ((UInt32*)aesctr, buf, len/AES_BLOCK_SIZE);
            return 0;
        }
        switch (mode) {
        case CTR: return ctr_decrypt (buf, buf, len, &ctr);
        case CFB: return cfb_decrypt (buf, buf, len, &cfb);
        default:  abort();
        }
    }

    int done()
    {
        if (aesctr)
        {
            return 0;
        }
        switch (mode) {
        case CTR: return ctr_done (&ctr);
        case CFB: return cfb_done (&cfb);
        default:  abort();
        }
    }
};

// Найти номер режима шифрования по его имени
int find_mode (char *name)
{
    if (strequ(name,"ctr"))  return  0;
    if (strequ(name,"cfb"))  return  1;
    else                     return -1;
}


/*-------------------------------------------------*/
/* Пользовательские функции                        */
/*-------------------------------------------------*/

// Generate key from password and salt using numIterations of sha-512 hashing (PKCS5#2)
void Pbkdf2Hmac (const BYTE *pwd, int pwdSize, const BYTE *salt, int saltSize,
                 int numIterations, BYTE *key, int keySize)
{
    int hash = find_hash("sha512");
    unsigned long ulKeySize = keySize;
    pkcs_5_alg2 (pwd, pwdSize, salt, saltSize, numIterations, hash, key, &ulKeySize);
}

// Зашифровывает или расшифровывает поток данных, в зависимости от значения DoEncryption
int docrypt (enum TEncrypt DoEncryption, int cipher, int mode, BYTE *key, int keysize, int rounds, BYTE *iv,
             CALLBACK_FUNC *callback, void *auxdata)
{
    EncryptionMode encryptor(mode);
    encryptor.start (cipher, iv, key, keysize, rounds);

    int InSize = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;  // количество прочитанных байт или код ошибки
    int RemainderSize = 0;                           // необработанный остаток предыдущего блока (всегда 0 в нынешней реализации)
    BYTE* Buf0 = (BYTE*)malloc(LARGE_BUFFER_SIZE+AES_BLOCK_SIZE);    // место для данных, с учётом требований к выравниванию адреса буфера
    if (!Buf0)   goto Exit;                          // выход при нехватке памяти
   {BYTE* Buf = roundup_ptr (Buf0, AES_BLOCK_SIZE);

    while ( (InSize = callback ("read", Buf+RemainderSize, LARGE_BUFFER_SIZE-RemainderSize, auxdata)) >= 0 )  // выход при ошибке чтения
    {
        bool LastBlock = (InSize==0);               // True for the last block that should be processed in the special way
        if ((InSize+=RemainderSize)==0)     break;  // break if there's no more data

        int OutSize = encryptor.aesctr==NULL? InSize                        // process all input data unless optimized AES-CTR code used
                      : LastBlock? roundUp   (InSize, AES_BLOCK_SIZE)       // process more data if these are the last bytes in input stream
                                 : roundDown (InSize, AES_BLOCK_SIZE), x;   // process less data and move the rest to the next processing cycle

        DoEncryption==ENCRYPT
          ? encryptor.encrypt(Buf, OutSize)
          : encryptor.decrypt(Buf, OutSize);

        if (OutSize>InSize)  OutSize=InSize;

        if( (x=callback("write",Buf,OutSize,auxdata))<0 )   {InSize=x; break;}  // break on write rror
        if( LastBlock )                                     {InSize=0; break;}  // break if the last block
        RemainderSize = InSize-OutSize;
        // Перенесём необработанный остаток данных в начало буфера
        if (RemainderSize>0)                memmove (Buf, Buf+OutSize, RemainderSize);
    }}
Exit:
    encryptor.done();
    free (Buf0);
    return InSize;  // возвратим код ошибки или 0 если всё в порядке
}


/*-------------------------------------------------*/
/* Реализация класса ENCRYPTION_METHOD             */
/*-------------------------------------------------*/

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
ENCRYPTION_METHOD::ENCRYPTION_METHOD()
{
    cipher        = -1;
    mode          = -1;
    numIterations = 1000;
    rounds        = 0;
    keySize       = -1;
    fixed         = FALSE;
    strcpy(key,  "");
    strcpy(iv,   "");
    strcpy(salt, "");
    strcpy(code, "");
}

// Универсальный метод, отвечает на запросы "encryption?", "KeySize" и "IVSize"
int ENCRYPTION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
         if (strequ (what, "encryption?"))    return 1;               // Да, это алгоритм шифрования
    else if (strequ (what, "keySize"))        return keySize;         // Возвращает размер ключа, используемого в данном методе сжатия
    else if (strequ (what, "ivSize"))         return ivSize;          // Возвращает размер InitVector, используемого в данном методе сжатия
    else if (strequ (what, "numIterations"))  return numIterations;   // Возвращает количество итераций, используемых при генерации ключа по password+salt
    else                                      return COMPRESSION_METHOD::doit (what, param, data, callback);  // Передать остальные вызовы родительской процедуре
}

// Функция распаковки
int ENCRYPTION_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE key_bytes[MAXKEYSIZE];  fixed? decode16 (key, key_bytes) : buggy_decode16 (key, key_bytes);
    BYTE iv_bytes [MAXKEYSIZE];  fixed? decode16 (iv,  iv_bytes)  : buggy_decode16 (iv,  iv_bytes);
    return docrypt (DECRYPT, cipher, mode, key_bytes, strlen(key)/2, rounds, iv_bytes, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int ENCRYPTION_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE key_bytes[MAXKEYSIZE];  fixed? decode16 (key, key_bytes) : buggy_decode16 (key, key_bytes);
    BYTE iv_bytes [MAXKEYSIZE];  fixed? decode16 (iv,  iv_bytes)  : buggy_decode16 (iv,  iv_bytes);
    return docrypt (ENCRYPT, cipher, mode, key_bytes, strlen(key)/2, rounds, iv_bytes, callback, auxdata);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_ENCRYPTION)
void ENCRYPTION_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    sprintf (buf, "%s-%d/%s%s:n%d:r%d%s%s%s%s%s%s%s%s"
                                        , cipher_descriptor[cipher].name, keySize*8
                                        , EncryptionMode(mode).name()
                                        , fixed? ":f" : ""
                                        , numIterations
                                        , rounds
                                        , *key ?":k":"", key
                                        , *iv  ?":i":"", iv
                                        , *salt?":s":"", salt
                                        , *code?":c":"", code
                                        );
}

// Конструирует объект типа ENCRYPTION_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_ENCRYPTION (char** parameters)
{
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Делаем локальную копию, поскольку split портит строку
    char local_method[MAX_METHOD_STRLEN];
    strncopy (local_method, parameters[0], MAX_METHOD_STRLEN);

    // Разбиваем строку метода на максимум 2 части, разделённые знаком '/'
    // Это метод и режим шифрования (например, "aes/cfb")
    char *parts[3];
    split (local_method, '/', parts, 3);
    int mode    = parts[1]? find_mode(parts[1]) : 0;

    // Разбиваем строку метода на максимум 2 части, разделённые знаком '-'
    // После '-' может быть указан размер ключа в битах (например, "aes-128")
    split (local_method, '-', parts, 3);

    int cipher  = find_cipher(parts[0]);
    int keySize = parts[1]? parseInt (parts[1], &error)/8 : 0;
    if (mode<0 || cipher<0 || error)   return NULL;   // Это не метод ENCRYPTION

    ENCRYPTION_METHOD *p = new ENCRYPTION_METHOD;
    p->cipher  = cipher;
    p->mode    = mode;
    p->keySize = keySize? keySize : cipher_descriptor[cipher].max_key_length;
    p->ivSize  = cipher_descriptor[cipher].block_length;

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strequ (param, "f")) {
        p->fixed = TRUE; continue;
      }
      switch (*param) {                    // Параметры, содержащие значения
        case 'k':  strncopy (p->key,  param+1, sizeof (p->key));    continue;
        case 'i':  strncopy (p->iv,   param+1, sizeof (p->iv));     continue;
        case 's':  strncopy (p->salt, param+1, sizeof (p->salt));   continue;
        case 'c':  strncopy (p->code, param+1, sizeof (p->code));   continue;
        case 'n':  p->numIterations  = parseInt (param+1, &error);  continue;
        case 'r':  p->rounds         = parseInt (param+1, &error);  continue;
        default :  error=1;                                         continue;
      }
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
}

static int ENCRYPTION_x = AddCompressionMethod (parse_ENCRYPTION);   // Зарегистрируем парсер метода ENCRYPTION
