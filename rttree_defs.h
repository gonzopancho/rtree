/*      $NetBSD: $ */

/*-
 * Copyright (c) 2014 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Dennis Ferguson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *      
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * rttree_defs.h
 *
 * Common definitions used internal to the rttree library functions.
 * None of this should need to be externally visible.
 */
#ifndef	RTTREE_DEFS_H_
#define	RTTREE_DEFS_H_
#include <rttree.h>

/*
 * Dispatch stuff related to byte ordering first so if it is a
 * problem it will be easy to find.  We need to define RTT_ENDIAN
 * as 0x00 on a big-endian machine, 0x18 on a little-endian machine
 * and as 0x08 on a pdp-endian machine.  We also need a definition
 * of ntohl(x) which works on the machine.  (x) will do it in a
 * big-endian, the gcc builtin __builtin_bswap32((x)) will do on
 * a little-endian, but you are on your own for a pdp-endian.
 */
#include <machine/endian.h>

#ifndef BYTE_ORDER
	#error "I need to know the byte order!"
#endif	/* BYTE_ORDER */

#if	BYTE_ORDER == BIG_ENDIAN
#define		RTT_ENDIAN	0x00
#elif	BYTE_ORDER == LITTLE_ENDIAN
#define		RTT_ENDIAN	0x18
#elif	BYTE_ORDER == PDP_ENDIAN
#define		RTT_ENDIAN	0x08
#else
	#error "I don't know what BYTE_ORDER is set to!"
#endif

/*
 * Other platform stuff
 */
#ifdef notdef
/*
 * Memory barrier and uclz*() definitions for NetBSD
 */
#include <ulmath.h>
#include <sys/atomic.h>
#define RTT_WMB()	membar_producer()
#define RTT_RMB()	membar_consumer()

#else   /* notdef */

/*
 * Filler.  The __predict_*() macros and uclz*() are
 * defined in terms of gcc builtins.  The memory barriers
 * are defined as magic to tell the compiler "don't reorder
 * this".  The latter is sufficient for Intel processors,
 * which maintain read and write ordering out the other
 * side of the cache.  Other processors may need real code
 * if the volatile functions are to be used.
 */

#define RTT_WMB() __asm__ __volatile__ ("": : :"memory")
#define RTT_RMB() __asm__ __volatile__ ("": : :"memory")

#ifndef __predict_true
#define __predict_true(exp)     __builtin_expect((exp) != 0, 1)
#endif  /* __predict_true */

#ifndef __predict_false
#define __predict_false(exp)    __builtin_expect((exp) != 0, 0)
#endif  /* __predict_false */

#define uclz32(x)       __builtin_clz((x))

#endif	/* notdef */


/*
 * ALWAYS_INLINE and MAYBE_ALWAYS_INLINE definitions.  We use
 * ALWAYS_INLINE on the very short functions defined
 * below, while MAYBE_ALWAYS_INLINE is used on the longer
 * functions where there might be more reason to leave this
 * up to the compiler.
 *
 * I'd prefer to leave it all up to the compiler.
 */
#ifdef notdef
#define ALWAYS_INLINE   __attribute__((__always_inline__))
#else
#define ALWAYS_INLINE
#endif
#define MAYBE_ALWAYS_INLINE     ALWAYS_INLINE


/*
 * We customize code for volatile/non-volatile searches based
 * on RTTREE_VOLATILE being defined.  Here are some alternative
 * definitions that are well used.
 *
 * The generic rules for volatile code are:
 *
 * - Read each field used from an inode or node only once.
 *
 * - Access fields in rnodes and inodes through a pointer
 *   which has been declared volatile to coax the compiler
 *   to observe the above.
 *
 * - Don't assume that you know how you got where you are.  E.g.
 *   the fact that you reached an external node as an attached
 *   route does not mean it will still be attached to the inode
 *   you return to.
 */
#ifdef	RTTREE_VOLATILE
#define	V_		volatile
#define	V_RMB_()	/* nothing, mostly; a barrier on the DEC Alpha */
#else	/* RTTREE_VOLATILE */
#define	V_		/* nothing */
#define	V_RMB_()	/* nothing */
#endif	/* RTTREE_VOLATILE */


/*
 * Inlines to use when reading volatile rtt_inode_t *, rtt_ptr_t
 * and rtt_lut_t pointers from the structure.  These mostly do
 * nothing (i.e. return their argument as a result) but exist for
 * the benefit of the DEC Alpha which, in a volatile tree, requires
 * a memory barrier after reading a pointer but before dereferencing it.
 */
static inline rtt_ptr_t rv_ptr(rtt_ptr_t) ALWAYS_INLINE;

static inline rtt_ptr_t
rv_ptr (rtt_ptr_t p)
{
	V_RMB_();
	return (p);
}

static inline rtt_inode_t *rv_inode (rtt_inode_t *) ALWAYS_INLINE;

static inline rtt_inode_t *
rv_inode (rtt_inode_t *p)
{

	V_RMB_();
	return (p);
}

static inline rtt_lut_t rv_lut(rtt_lut_t) ALWAYS_INLINE;

static inline rtt_lut_t
rv_lut (rtt_lut_t p)
{

	V_RMB_();
	return (p);
}

/*
 * Some aliases for the node and inode structure members.  Here to
 * avoid burdening rttree.h with any avoidable namespace pollution.
 */
#define	RTT_LEFT	0
#define	RTT_RIGHT	1

#define	rtti_left	rtti_child[RTT_LEFT]	/* left inode child pointer */
#define	rtti_right	rtti_child[RTT_RIGHT]	/* right inode child pointer */

#define	rttn_next	rttn_union.rttu_next	/* node _next pointer */
#define	rttn_ptr	rttn_union.rttu_ptr	/* when key reached via ptr */
#define	rttn_key	rttn_union.rttu_key	/* when key directly reached */

/*
 * Some internal type definitions. rtt_ibit_t is theInternal version
 * of a bit number.  We read it into and manipulate it in an unsigned
 * int since this seems to generate tigher code most places.  An
 * unsigned int needs to be at least 16 bits long.  rtt_keyptr_t
 * is the type of an encoded pointer+key offset.  It needs to be
 * uint32_t to match the root structure storage definition.
 */
typedef unsigned int rtt_ibit_t;
typedef uint32_t rtt_keyptr_t;

/*
 * Stuff related to key and bit manipulation.  We search using a uint32_t
 * word size for key access so we can wire some parameters related to the
 * size of this.
 */
#define	RN_BYTE_BITS		0x18		/* bits selecting word byte */
#define	RN_BIT_BITS		0x07		/* bit in byte */

#define	RN_L2NBBY	3u			/* log2(bits in byte) */
#define	RN_NBBY		(1u << RN_L2NBBY)	/* bits in byte */

#define	RN_L2NBWD	(RN_L2NBBY + 2u)	/* log2(bits in word) */
#define	RN_NBWD		(1u << RN_L2NBWD)		/* bits in word */

#define	RN_HI_BYTE_SHIFT	(RN_NBWD - RN_NBBY)

#define	RN_BITMASK	(RN_NBWD - 1)		/* mask for bits in word */
#define	RN_BITWORD	(~(rtt_ibit_t) RN_BITMASK)
#define	RN_NOBIT	RN_BITWORD

#define	RN_WORD(bit)	((bit) >> RN_L2NBWD)
#define	RN_BYTE(bit)	((bit) >> RN_L2NBBY)

#define	RN_BIT(bit)	((bit) & RN_BIT_BITS)
#define	RN_MAXBIT(bit)	((bit) | RN_BIT_BITS)
#define	RN_MINBIT(bit)	((bit) & (~(rtt_ibit_t) RN_BIT_BITS))

/*
 * The maximum prefix length is the maximum we can fit in
 * an rtt_bit_t, less 1 word.
 */
#define	RN_MAX_PLENGTH	((1u << (sizeof(rtt_bit_t) * RN_NBBY)) - RN_NBWD)

/*
 * rn_same_byte()
 *
 * Return true if the two bit numbers test bits in the same byte.
 */
static inline bool
rn_same_byte (rtt_ibit_t b1, rtt_ibit_t b2)
{

	return ((b1 ^ b2) <= RN_BIT_BITS);
}

/*
 * rn_word_byte()
 *
 * Given a word fetch the byte having the bit number.
 */
static inline uint8_t
rn_word_byte(uint32_t, rtt_ibit_t) ALWAYS_INLINE;

static inline uint8_t
rn_word_byte (uint32_t word, rtt_ibit_t bit)
{

	/*
	 * Figure out the bit shift.
	 */
	bit ^= (RN_BYTE_BITS ^ RTT_ENDIAN);
	bit &= RN_BYTE_BITS;

	/*
	 * Shift the required byte to the low order and return it.
	 */
	return ((uint8_t) (word >> bit));
}


/*
 * rn_key_byte()
 *
 * Given a key pointer fetch the byte from the key which has the
 * bit number.
 */
static inline uint8_t
rn_key_byte(const uint32_t *, rtt_ibit_t) ALWAYS_INLINE;

static inline uint8_t
rn_key_byte (const uint32_t *kp, rtt_ibit_t bit)
{
	uint32_t word;

	/*
	 * Get the word from the key.  Return the result.
	 */
	word = kp[RN_WORD(bit)];
	return (rn_word_byte(word, bit));
}


/*
 * rn_word_idx()
 *
 * Given a word, a bit number and an LUT size, return the
 * index into the LUT.
 */
static inline rtt_ibit_t
rn_word_idx(uint32_t, rtt_ibit_t, rtt_ibit_t) ALWAYS_INLINE;

static inline rtt_ibit_t
rn_word_idx (uint32_t word, rtt_ibit_t bit, rtt_ibit_t lut_size)
{

	/*
	 * Shift the bit indicated by `bit' to the high order end
	 * of the word, then shift right by 3 bytes plus the
	 * lut_size to place the correct number of bits at
	 * the low order end.  We ignore everything in `bit'
	 * beyond the low order 5 bits.
	 */
	word <<= ((bit ^ RTT_ENDIAN) & RN_BITMASK);
	word >>= (RN_HI_BYTE_SHIFT + (lut_size & RTT_LUT_MASK));

	return ((rtt_ibit_t) word);
}


/*
 * rn_key_idx()
 *
 * Given a key pointer, a bit number and a LUT size, return the
 * index into the LUT.
 */
static inline rtt_ibit_t
rn_key_idx(const uint32_t *, rtt_ibit_t, rtt_ibit_t) ALWAYS_INLINE;

static inline rtt_ibit_t
rn_key_idx (const uint32_t *kp, rtt_ibit_t bit, rtt_ibit_t lut_size)
{
	uint32_t word;

	/*
	 * Get the word from the key.  Return the result.
	 */
	word = kp[RN_WORD(bit)];

	return (rn_word_idx(word, bit, lut_size));
}


/*
 * rn_key_bit()
 *
 * Given a key pointer and a bit number, return the value of that bit.
 */
static inline rtt_ibit_t
rn_key_bit (const uint32_t *kp, rtt_ibit_t bit)
{
	uint32_t word;

	word = kp[RN_WORD(bit)];
	bit &= RN_BITMASK;
	bit ^= (RN_BITMASK ^ RTT_ENDIAN);
	return ((word >> bit) & 0x1);
}


/*
 * rn_cmp()
 *
 * Given two key pointers and the length of the shorter, compare
 * them to find the first bit of difference.
 */
static inline rtt_ibit_t
rn_cmp (const uint32_t *kp1, const uint32_t *kp2, rtt_ibit_t p_len)
{
	rtt_ibit_t i;

	for (i = 0; i < p_len; i += RN_NBWD) {
		uint32_t k_diff;

		k_diff = *kp1 ^ *kp2;
		if (k_diff) {
			i += (rtt_ibit_t) uclz32(ntohl(k_diff));
			break;
		}
		kp1++; kp2++;
	}

	return ((i <= p_len) ? i : p_len);
}


/*
 * Macros for single-bit searches and tests.  These only load
 * a key word if the word isn't the same as the last that was
 * tested.  The result, returned in the first argument, is
 * the state of the bit.
 *
 * RN_KEYBIT() does the work.  bit is the bit to test, kbit
 * is returned as its value.  k and which_k must be lvalues,
 * they are accessed several times.
 */
#define	RN_KEYBIT(kbit, bit, kp, k, which_k)				\
	do {								\
		rtt_ibit_t Xbit = (bit);				\
		Xbit ^= which_k;					\
		if (Xbit > RN_BITMASK) {				\
			which_k ^= (Xbit & RN_BITWORD);			\
			k = (kp)[RN_WORD(which_k)];			\
			Xbit &= RN_BITMASK;				\
		}							\
		(kbit) = (k >> Xbit) & 0x1;				\
	} while (0)

/*
 * RN_KEYBIT_INIT() initializes k and which_k.  `bit' is
 * the first bit we're likely to test.
 */
#define	RN_KEYBIT_INIT(bit, kp, k, which_k)				\
	do {								\
		const rtt_ibit_t Xbit = (bit);				\
		which_k = (Xbit & RN_BITWORD) | (RTT_ENDIAN ^ RN_BITMASK); \
		if (Xbit == RN_NOBIT) { 				\
			k = 0; 						\
		} else { 						\
			k = (kp)[RN_WORD(Xbit)];			\
		}							\
	} while (0)

/*
 * Like the above macros, but for an LUT index search.
 * The returned value is a uint32_t word with the bits required
 * for an LUT index shifted to the high order end
 */
#define	RN_KEYLUT(k_word, bit, kp, k, which_k)				\
	do {								\
		rtt_ibit_t Xbit = (bit);				\
		Xbit ^= which_k;					\
		if (Xbit > RN_BITMASK) {				\
			which_k ^= (Xbit & RN_BITWORD);			\
			k = (kp)[RN_WORD(which_k)];			\
			Xbit &= RN_BITMASK;				\
		}							\
		(k_word) = (k << Xbit);					\
	} while (0)

/*
 * RN_KEYLUT_INIT() initializes k and which_k
 */
#define	RN_KEYLUT_INIT(bit, kp, k, which_k)				\
	do {								\
		const rtt_ibit_t Xbit = (bit);				\
		which_k = (Xbit & RN_BITWORD) | RTT_ENDIAN;		\
		if (Xbit == RN_NOBIT) {					\
			k = 0;						\
		} else {						\
			k = (kp)[RN_WORD(Xbit)];			\
		}							\
	} while (0)


/*
 * RN_KEYLUT_TO_KEYBIT() modifies a `which_k' initialized for RN_KEYLUT()
 * to be used by RN_KEYBIT().  which_k needs to be an lvalue.
 */
#define	RN_KEYLUT_TO_KEYBIT(which_k)					\
	do {								\
		which_k ^= RN_BITMASK;					\
	} while (0)

/*
 * RN_KEYBIT_TO_KEYLUT() does the reverse of the above.
 */
#define	RN_KEYBIT_TO_KEYLUT(which_k)	RN_KEYLUT_TO_KEYBIT(which_k)

/*
 * RN_NOT() is a macro to reverse the direction of a one-bit branch.
 */
#define	RN_NOT(x)	((x) ^ 0x01)

/*
 * rn_cmp_ext()
 *
 * Compare two external nodes.  Return whether the first should
 * be considered LT, EQ or GT the second.
 */
static inline int
rn_cmp_ext (rttree_t *rr, rtnode_t *rn1, rtnode_t *rn2)
{
	const rtt_colormask_t mask = rr->rttr_color_mask;
	rtt_colormask_t c1, c2;
	rtt_cmpf_t cmp;

	/*
	 * Do a color comparison first.
	 */
	c1 = (rn1->rttn_color ^ mask) & (mask >> 16);
	c2 = (rn2->rttn_color ^ mask) & (mask >> 16);
	if (c1 != c2) {
		if (c1 < c2) {
			return (RTT_CMP_LT);
		}
		if (c1 > c2) {
			return (RTT_CMP_GT);
		}
	}

	/*
	 * Use comparison function.  If there isn't one they
	 * are equal.
	 */
	cmp = rr->rttr_cmp;
	if (cmp) {
		return (cmp(rr, rn1, rn2));
	}
	return (RTT_CMP_EQ);
}


/*
 * Pointer and LUT manipulation defines.
 */
#define	RP_NULL		((rtt_ptr_t) NULL)	/* a NULL rtt_ptr_t */
#define	RP_INODE	0x1			/* the inode bit */

#define	RTT_LUT_1	8		/* pseudo LUT size for internal use */

#define	RI_LUT_MASK	0x7		/* mask for LUT in pointer */
#define	RI_LUT(lut)	((lut) & RI_LUT_MASK)


/*
 * Use inlines for conversions so we get some type checking.
 */

/*
 * rp_is_int()
 *
 * Determine if an rtt_ptr_t value is an internal node pointer.
 */
static inline bool rp_is_int(rtt_ptr_t) ALWAYS_INLINE;

static inline bool
rp_is_int (rtt_ptr_t rp)
{
	return ((rp & RP_INODE) != 0);
}


/*
 * rp_to_int()
 *
 * Given an rtt_ptr_t, return an inode pointer.
 */
static inline rtt_inode_t *rp_to_int(rtt_ptr_t) ALWAYS_INLINE;

static inline rtt_inode_t *
rp_to_int (rtt_ptr_t rp)
{
	return ((rtt_inode_t *) (rp ^ RP_INODE));
}


/*
 * rp_from_int()
 *
 * Given an inode pointer, produce an rtt_ptr_t.
 */
static inline rtt_ptr_t rp_from_int(rtt_inode_t *) ALWAYS_INLINE;

static inline rtt_ptr_t
rp_from_int (rtt_inode_t *ri)
{
	return (((rtt_ptr_t) ri) | RP_INODE);
}


/*
 * rp_is_ext()
 *
 * Determine if an rtt_ptr_t value is an external node pointer.
 */
static inline bool rp_is_ext(rtt_ptr_t) ALWAYS_INLINE;

static inline bool
rp_is_ext (rtt_ptr_t rp)
{
	return ((rp & RP_INODE) == 0);
}


/*
 * rp_to_ext()
 *
 * Given an rtt_ptr_t, return an external node pointer
 */
static inline rtnode_t * rp_to_ext(rtt_ptr_t) ALWAYS_INLINE;

static inline rtnode_t *
rp_to_ext (rtt_ptr_t rp)
{
	return ((rtnode_t *) rp);
}

/*
 * rp_from_ext()
 *
 * Given a pointer to an external node, return an rtt_ptr_t.
 */
static inline rtt_ptr_t rp_from_ext(rtnode_t *) ALWAYS_INLINE;

static inline rtt_ptr_t
rp_from_ext (rtnode_t *rn)
{
	return ((rtt_ptr_t) rn);
}


/*
 * lut_make()
 *
 * Given an rtt_ptr_t array pointer and an rtt_size_t style
 * array length, return an rtt_lut_t.
 */
static inline rtt_lut_t
lut_make(rtt_ptr_t *, rtt_size_t) ALWAYS_INLINE;

static inline rtt_lut_t
lut_make (rtt_ptr_t *lp, rtt_size_t lut_len)
{
	return ((rtt_lut_t) lp | lut_len);
}


/*
 * lut_size()
 *
 * Given an LUT pointer return the size of the array as an rtt_size_t.
 */
static inline rtt_size_t lut_size(rtt_lut_t) ALWAYS_INLINE;

static inline rtt_size_t
lut_size (rtt_lut_t lut)
{
	return ((rtt_size_t) RI_LUT(lut));
}


/*
 * lut_isize()
 *
 * Like the above, but return the complement in an rtt_ibit_t
 * instead.
 */
static inline rtt_ibit_t lut_isize(rtt_lut_t) ALWAYS_INLINE;

static inline rtt_ibit_t
lut_isize (rtt_lut_t lut)
{
	return ((rtt_ibit_t) RI_LUT(~lut));
}


/*
 * lut_ishift()
 *
 * Another version of the above.  Return the size with 24 added to it.
 */
static inline rtt_ibit_t lut_ishift(rtt_lut_t) ALWAYS_INLINE;

static inline rtt_ibit_t
lut_ishift (rtt_lut_t lut)
{
	return ((rtt_ibit_t) RI_LUT(lut) + RN_HI_BYTE_SHIFT);
}



/*
 * lut_ptr()
 *
 * Given an LUT pointer return the rtt_ptr_t array pointer.
 */
static inline rtt_ptr_t *lut_ptr(rtt_lut_t) ALWAYS_INLINE;

static inline rtt_ptr_t *
lut_ptr (rtt_lut_t lut)
{
	return ((rtt_ptr_t *)(lut & ~RTT_LUT_MASK));
}


/*
 * lut_align_ok()
 *
 * Return true if the offered pointer is well aligned for an LUT.
 */
static inline bool lut_align_ok(rtt_ptr_t *) ALWAYS_INLINE;

static inline bool
lut_align_ok (rtt_ptr_t *ptr)
{
	return (((rtt_lut_t) ptr & RTT_LUT_MASK) == 0);
}


/*
 * The rtt_keyptr_t is encoded with the pointer offset in the upper
 * 16 bits and the key offset in the lower 16 bits.  If the key is
 * in the same structure as the rtnode_t, which is likely the
 * common case, then the pointer offset is zero and the value can be
 * used directly as a key offset, otherwise we'll need to shift out
 * the pointer offset and mask out the key.
 */
#define	RN_MIN_OFFSET \
	(offsetof(rtnode_t, rttn_parent) + sizeof(rtt_inode_t *))

#define	RN_PTR_SHIFT		16
#define	RN_PTR_SIZE		sizeof(uint32_t *)
#define	RN_PTR_OFFMASK		(RN_PTR_SIZE - 1)
#define	RN_PTR_MAX \
		(size_t)(((1u << RN_PTR_SHIFT) - 1) * RN_PTR_SIZE)
#define	RN_PTR_ALIGN_OK(x)	(((x) & RN_PTR_OFFMASK) == 0)

#define	RN_KEY_SIZE		sizeof(uint32_t)
#define	RN_KEY_OFFMASK		(RN_KEY_SIZE - 1)
#define	RN_KEY_MAX \
		(size_t)(((1u << RN_PTR_SHIFT) - 1) * RN_KEY_SIZE)
#define	RN_KEY_ALIGN_OK(x)	(((x) & RN_KEY_OFFMASK) == 0)



/*
 * rn_key()
 *
 * Given a tree root and a node, return a pointer to the node's key.
 */
static inline const uint32_t *
rn_key (const rttree_t *, const rtnode_t *) ALWAYS_INLINE;

static inline const uint32_t *
rn_key (const rttree_t *rr, const rtnode_t *rn)
{
	rtt_keyptr_t pk = rr->rttr_offsets;
	
	if (pk <= RN_KEY_MAX) {
		/*
		 * Key offset only.
		 */
		return (&rn->rttn_key[pk]);
	}

	/*
	 * Pointer and key offset
	 */
	return (&((rn->rttn_ptr[pk >> RN_PTR_SHIFT])[pk & RN_KEY_OFFMASK]));
}


/*
 * rn_first()
 *
 * Given an external node, return the first node with the same key.
 * In a volatile tree this can return NULL.
 */
static inline rtnode_t *rn_first (const rttree_t *, rtnode_t *) ALWAYS_INLINE;

static inline rtnode_t *
rn_first (const rttree_t *rr, rtnode_t *rn)
{
	V_ rtt_inode_t *ri;
	V_ rtnode_t *v_rn;
	rtt_ibit_t bit, dir;
	rtt_ptr_t r_ptr;

	v_rn = rn;
	ri = rv_inode(v_rn->rttn_parent);
	bit = ri->rtti_bit;
	if (bit == rn->rttn_len) {
		return (rp_to_ext(rv_ptr(ri->rtti_attached)));
	}
	dir = rn_key_bit(rn_key(rr, rn), bit);
	r_ptr = ri->rtti_child[dir];
	if (!rp_is_ext(r_ptr)) {
		r_ptr = RP_NULL;
	}
	return (rp_to_ext(rv_ptr(r_ptr)));
}



/*
 * rn_color_check()
 *
 * Given a color and two color masks determine a comparison result.
 * RTT_CMP_GT is returned if the ignore color operation is non-zero,
 * otherwise RTT_CMP_LT is returned if the skip color operation is
 * non-zero, otherwise if both operations returned zero we return
 * RTT_CMP_EQ.
 */
static inline int
rn_color_check(rtt_colormask_t, rtt_colormask_t, rtt_colormask_t) ALWAYS_INLINE;

static inline int
rn_color_check (rtt_colormask_t color, rtt_colormask_t c_ignore,
		rtt_colormask_t c_skip)
{

	if (((color ^ c_ignore) & (c_ignore >> RTT_COLOR_SHIFT)) != 0) {
		return (RTT_CMP_GT);
	}
	if (((color ^ c_skip) & (c_skip >> RTT_COLOR_SHIFT)) != 0) {
		return (RTT_CMP_LT);
	}
	return (RTT_CMP_EQ);
}


/*
 * rn_func_check()
 *
 * Match a node against user-specified stuff.  Returns RTT_CMP_LT
 * if the node should be treated as a skip, RTT_CMP_GT if the node
 * should be treated as an ignore, and RTT_CMP_EQ if the node is
 * a match.
 */
static inline int
rn_func_check(rttree_t *, rtnode_t *, const void *) ALWAYS_INLINE;

static inline int
rn_func_check (rttree_t *rr, rtnode_t *rn, const void *gorp)
{
	rtt_matchf_t m_func = rr->rttr_match;

	if (m_func) {
		return (m_func(rr, rn, gorp));
	}
	return (RTT_CMP_EQ);
}


/*
 * rn_match()
 *
 * Do both operations above.
 */
static inline int
rn_match(rttree_t *, rtnode_t *, bool, rtt_colormask_t, rtt_colormask_t,
	 bool, const void *) ALWAYS_INLINE;

static inline int
rn_match (rttree_t *rr, rtnode_t *rn, bool has_color, rtt_colormask_t c_ignore,
	  rtt_colormask_t c_skip, bool has_gorp, const void *cmp_gorp)
{
	int res = RTT_CMP_EQ;

	if (has_color) {
		rtt_colormask_t color = rn->rttn_color;

		res = rn_color_check(color, c_ignore, c_skip);
	}

	if (has_gorp && res == RTT_CMP_EQ) {
		res = rn_func_check(rr, rn, cmp_gorp);
	}

	return (res);
}

#endif	/* RTTREE_DEFS_H_ */
