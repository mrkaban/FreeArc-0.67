#ifndef SIPHASH_IMPL_H
#define SIPHASH_IMPL_H

#if defined(_MSC_VER)
	#include <intrin.h>

	#define INLINE __forceinline
	#define ROTL64(a,b) _rotl64(a,b)
	#define MM16 __declspec(align(16))

	typedef unsigned int uint32_t;
	typedef unsigned __int64 uint64_t;

	#if (_MSC_VER >= 1500)
		#define SSSE3_INTRINSICS
	#endif
	#if (_MSC_VER > 1200) || defined(_mm_free)
		#define SSE2_INTRINSICS
	#endif
#else
	#include <stdint.h>
	#include <stdlib.h>

	#define INLINE __attribute__((always_inline))
	#define ROTL64(a,b) (((a)<<(b))|((a)>>(64-b)))
	#define MM16 __attribute__((aligned(16)))
#endif

#if defined(__SSE2__) || defined(SSE2_INTRINSICS)
	#include <emmintrin.h>
	typedef __m128i xmmi;
	typedef __m64 qmm;

	typedef union packedelem64_t {
		uint64_t u[2];
		xmmi v;
	} packedelem64;

	typedef union packedelem8_t {
		unsigned char u[16];
		xmmi v;
	} packedelem8;
#endif

#if defined(__SSSE3__) || defined(SSSE3_INTRINSICS)
	#include <tmmintrin.h>
#endif

#endif // SIPHASH_IMPL_H
