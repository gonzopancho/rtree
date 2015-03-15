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
 * rttree.h - A pretty good longest match route tree lookup
 */
#ifndef RTTREE_H_
#define	RTTREE_H_
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*
 * An rttree is a structure intended to support the storage
 * of data items (nodes) associated with variable-length keys in
 * a way which allows efficient longest-matching prefix lookups.
 * It is suitable, by design, for use as a route lookup structure
 * for protocols like IPv4 and IPv6.  Some lookup operations can
 * be performed concurrently with in-progress modifications to
 * an rttree without explicit locking.  Search and node keys
 * (that is, address prefixes) are assumed to be stored in network
 * byte order, having big-endian bit significance at the byte address
 * level, but are required to be 4-byte aligned and to occupy an even
 * multiple of 4 bytes of storage, that being the key bit
 * length rounded up to an even multiple of 32.  Essentially keys
 * are treated as uint32_t arrays, with an unnatural bit search
 * precedence on hosts having little-endian data storage.
 *
 * While nodes are primarily distinguished by key value and length
 * (i.e. by prefix), an rttree optionally allows the storage of
 * multiple nodes with the same key value and length in the structure.
 * To take advantage of this the user must initialize the rttree
 * with pointers to two functions returning the results of lexical
 * comparisons (RTTREE_LT, RTTREE_EQ, RTTREE_GT), one which takes
 * a pair of node arguments to compare which is used for node adds
 * and other structure modifications, and one which takes a single
 * node plus a pointer to a set of user-defined datums to compare
 * to the node.  Nodes are sorting into a list in "ascending"
 * order (i.e. the first node in the list is RTTREE_LT compared
 * to all others); when several nodes exist having the same longest
 * match prefix found during a search by key value alone it is
 * the first node in the list which is returned.  Nodes which
 * compare RTTREE_EQ to other nodes are not permitted in the
 * tree; if no comparison functions are provided at rttree
 * initialization then only a single node having a particular
 * prefix is permitted.
 *
 * While the ordering of same-prefix nodes must ultimately depend on
 * information about the node not visible to the rttree package (hence
 * the comparison functions), function calls may be minimized by the
 * definition of a node "color".  A color is a 16 bit bit-encoded integer
 * value associated with a node whose encoding is (almost fully) defined
 * by the user.  An arbitrary subset of the colar bits may be defined
 * at rttree initialization time as indicating ordering (i.e. a compare
 * function call will only be made if two nodes have the same subset of
 * color bits), and different subsets may be used for constructing
 * searches.
 *
 * Operations which modify an rttree may be required to allocate
 * memory to perform their operation.  Two different types of allocations
 * are required, one to allocate inodes (type rtt_inode_t) and one
 * to allocate lookup tables, which are power-of-2 length arrays of
 * between 4 and 256 pointer-sized integers (type rtt_lut_t).  The
 * user of the package is required to provide, at rttree initialization
 * time, a set of functions to handle the allocation and freeing of these
 * structures.  Two function handles must be provided to free each
 * structure type, one which is called when the memory being freed has
 * not recently been included in the lookup structure and may be modified
 * or released to the free pool immediately and one which is called to
 * release memory recently removed from the structure.  In the latter
 * case references to the memory being released may still be held by
 * threads with a concurrent lookup in progress (should this be allowed)
 * and must be held unmodified until it can be proven that all processes
 * which might have initiated a concurrent lookup before the modification
 * have completed.  This latter constraint must be dealt with by logic
 * outside the rttree package since the package itself has no explicit
 * information about concurrent lookups and simply performs modifications
 * in a way which guarantees concurrent lookups will operate correctly.
 * Also note that, to provide update atomicity, the package stores
 * additional information in the low order bits of pointers.  This
 * requires that inodes, lookup tables and the tree root structure
 * (type rttree_t) be allocated such that the three lowest order bits
 * of the address pointer are zero-valued.  Nodes (type rtnode_t),
 * on the other hand, require only that the single lowest order bit
 * of the address be zero-valued, something which, on conventional
 * CPU architectures, should be guaranteed by the alignment constraints
 * of internal structure members.
 *
 * The package operates in a way that results in two almost-separate
 * search structures being built.  One of these is a fairly conventional
 * radix trie structure (of the four pointer inode variety) which uses
 * inodes and requires O(log(N)) single-bit tests and longest-match
 * search comparisons to find a result.  The second uses lookup tables
 * (type rtt_lut_t above) to branch on up to 8 bits at a time and provides
 * a highly optimized longest-match search with a cost depending only
 * on the number of overlapping prefixes.  Both search structures
 * provide equivalent results, so the latter is used whenever possible
 * (though its construction depends on the scaffolding provided by
 * the former).  This has several implications.  First, while the
 * failure to allocate an inode will result in the failure of the
 * operation which attempted to do so, a failure to allocate a
 * lookup table may not since the consequence of the failure is only
 * sub-optimal search performance.  Second, the cost of this higher
 * performance lookup is that certain modifications of the tree
 * (particularly those involving short length prefixes) can, unlike
 * a conventional radix trie, be relatively expensive to compute in
 * very large trees.  The package does what it can to minimize the
 * latter but there definitely is no free lunch; the rttree is
 * most suitable when the rate at which lookups are done greatly
 * exceeds the rate at which modifications to the structure are
 * performed.
 *
 * A final note concerns the guarantee which the rttree endeavours
 * to make to threads doing lookups in a tree being concurrently
 * modified, that is a "volatile" tree.  Modifications to this
 * structure are not atomic; occassionally a modification will require
 * many writes to the structure to complete.  What is guaranteed for
 * basic lookups, however, is that any individual lookup done in a
 * volatile tree undergoing a series of modifications will see the
 * same result as the same lookup done in a non-volatile tree at some
 * point during the series.  That is, a lookup will find an old
 * result or a new result, but nothing else.  That said, if several
 * different lookups are performed in different threads, or even
 * sequentially in the same thread, it is not guaranteed that the
 * different lookups will uniformly see either an old or a new
 * result.  The tree may be in a state where different lookups
 * will receive results from different generations of the tree
 * for some time.  This behaviour does not generally bother applications
 * doing networking address lookups (distributed routing often
 * behaves the same way) but may not be appropriate for other
 * applications.  Also note that particular lookups may require
 * somewhat different, slightly more costly, approaches to find a
 * result than would be necessary in a rttree known to be unchanging.
 * To allow full advantage of the structure to be taken in either case
 * alternate search functions are often provided to use in volatile
 * and non-volatile trees.
 *
 * Note that the remaining annoyance with this structure are the _byte
 * and _count[] members of the inode, which are used for bookkeeping
 * to help build the LUT superstructure but which aren't useful for
 * searches.  The problem with them is that their presence in memory
 * which is accessed by concurrent searchers causes unnecessary cache
 * flushes of inodes in which the data actually used by searchers is not
 * going to change.  This could be fixed by maintaining that data in
 * separate memory, but this was an added complexity I didn't want to
 * to deal with...
 */

/*
 * An rtt_ptr_t can point at either an internal node (an rtt_inode_t)
 * or an external node (an rtnode_t), or nothing (i.e. be NULL).
 * When it points at an internal node its low order bit is set.  It
 * gets cast to the appropriate pointer type with the low order bit
 * cleared.
 *
 * An rtt_lut_t points at a "lookup table", a power-of-2 sized array
 * of rtt_ptr_t's.  Its lowest order 3 bits encode the size of the
 * table, these are cleared when it is cast to an array pointer (an
 * rtt_ptr_t *).
 *
 * An rtt_bit_t holds a bit-to-test number.  0 is the most significant bit.
 *
 * An rtt_color_t is a 16 bit value used to distinguish routes.  Its use
 * is up to the application.
 *
 * An rtt_size_t holds a size index for an LUT.  It holds a value
 * between 0 and 7 inclusive, where the number of pointers in the
 * LUT is (1 << (8 - size)).  See RTT_LUT_* below.
 */
typedef uintptr_t rtt_ptr_t;
typedef uintptr_t rtt_lut_t;
typedef uint16_t  rtt_bit_t;
typedef uint16_t  rtt_color_t;
typedef uint32_t  rtt_colormask_t;
typedef struct rtt_inode_ rtt_inode_t;
typedef struct rtnode_  rtnode_t;
typedef uint8_t   rtt_size_t;

/*
 * An internal node.
 */
struct rtt_inode_ {
	rtt_ptr_t	rtti_child[2];		/* left and right children */
	rtt_lut_t	rtti_lut;		/* lookup table for byte */
	rtt_bit_t	rtti_bit;		/* bit to test/prefix len */
	uint8_t		rtti_byte;		/* byte for lut part of key */
	uint8_t		rtti_counts[5];		/* counts of nodes below */
	rtt_inode_t	*rtti_parent;		/* parent inode */
	rtt_inode_t	*rtti_aparent;		/* inode parent - attached */
	rtt_ptr_t	rtti_attached;		/* attached node, if any */
};


/*
 * An external node.
 */
struct rtnode_ {
	union {
		rtt_ptr_t rttu_next;		/* next external in chain */
		const uint32_t * const rttu_ptr[1];	/* pointer access */
		const uint32_t rttu_key[1];	/* key access */
	} rttn_union;
	rtt_bit_t	rttn_len;		/* bit length of key */
	rtt_color_t	rttn_color;		/* the node "color" */
	rtt_inode_t *rttn_parent;		/* inode we're pointed from */
};


/*
 * Number of lookup table sizes.  2-256 pointers in powers of 2.
 * Only 4-256 are real LUTs though.
 */
#define	RTT_LUT_SIZES	8
#define	RTT_LUT_NUM	(RTT_LUT_SIZES - 1)

/*
 * Coding of our lookup table size allocations is a bit strange.
 * The decoding key for the number of pointers (type rtt_ptr_t)
 * in an lut is:
 *
 *     0    1    2    3    4    5    6    7
 *    256  128  64   32   16    8    4    2
 *
 * 7 (i.e. 2 pointers) is a pseudo-LUT.  We don't allocate memory
 * for that, it points an an inode's pointers instead.
 */
#define RTT_LUT_256	(0u)
#define RTT_LUT_128	(1u)
#define RTT_LUT_64	(2u)
#define RTT_LUT_32	(3u)
#define RTT_LUT_16	(4u)
#define RTT_LUT_8	(5u)
#define RTT_LUT_4	(6u)
#define RTT_LUT_2	(7u)
#define	RTT_LUT_MASK	((rtt_lut_t) RTT_LUT_2)	/* only these bits used */

/*
 * Macro to determine the number of LUT pointers given a size.
 */
#define	RTT_LUT_ENTRIES(lut)	(1 << (8 - ((lut) & RTT_LUT_MASK)))

/*
 * Forward declaration
 */
typedef struct rttree_ rttree_t;

/*
 * Node comparison functions are passed to the initialization
 * routine in a structure but are recorded directly in the
 * root structure.  These are the return values.
 */
#define	RTT_CMP_LT	(-1)
#define	RTT_CMP_EQ	(0)
#define	RTT_CMP_GT	(1)

/*
 * These are the declarations.  All calls take a thunk argument
 * in case the called function needs context.
 */
typedef int (*rtt_cmpf_t)(const rttree_t *, const rtnode_t *, const rtnode_t *);
typedef int (*rtt_matchf_t)(const rttree_t *, const rtnode_t *, const void *);

struct rtt_cmpfuncs_ {
	rtt_cmpf_t	rttc_cmp;
	rtt_matchf_t	rttc_match;
};
typedef struct rtt_cmpfuncs_ rtt_cmpfuncs_t;

/*
 * The memory allocation procedure handle structure.  A pointer
 * to one of these is provided to the function initializing
 * the tree root structure.
 */
struct rtt_alloc_ {
	rtt_inode_t *(*rtta_inode_alloc)(rttree_t *);
	void	(*rtta_inode_free)(rttree_t *, rtt_inode_t *);
	void	(*rtta_inode_unref)(rttree_t *, rtt_inode_t *);
	rtt_ptr_t *(*rtta_lut_alloc)(rttree_t *, rtt_size_t);
	void	(*rtta_lut_free)(rttree_t *, rtt_size_t, rtt_ptr_t *);
	void	(*rtta_lut_unref)(rttree_t *, rtt_size_t, rtt_ptr_t *);
};
typedef struct rtt_alloc_ rtt_alloc_t;


/*
 * The stats structure.  Keeps track of the counts of things in a tree.
 */
struct rtt_stats_ {
	uint64_t	rtts_adds;	/* route adds */
	uint64_t	rtts_deletes;	/* route deletes */
	uint64_t	rtts_changes;	/* route changes */
	uint32_t	rtts_luts[RTT_LUT_NUM];  /* count of LUTs, by size */
	uint32_t	rtts_inodes;	/* inodes in tree */
	uint32_t	rtts_dests;	/* unique destinations */
	uint32_t	rtts_nodes;	/* total externals */
};
typedef struct rtt_stats_ rtt_stats_t;

/*
 * The optimization struct contains low and high watermarks for
 * LUT additions and deletions.  The pointers we pass around
 * point at arrays of RTT_LUT_NUM (7) of these structures.
 */
typedef struct {
	uint8_t		hi;		/* the high watermark */
	uint8_t		lo;		/* the low watermark */
} rtt_opt_t;

/*
 * The tree root structure.  Used to track our data structure.
 */
struct rttree_ {
	rtt_inode_t	rttr_root;		/* root inode in structure */
	uint32_t	rttr_offsets;		/* pointer & key offsets */
	rtt_colormask_t	rttr_color_mask;	/* mask for <> color cmp */
	rtt_size_t	rttr_root_lut;		/* size of (fixed) root lut */
	const rtt_opt_t	*rttr_opt;		/* optimization watermarks */
	rtt_cmpf_t	rttr_cmp;		/* function to compare nodes */
	rtt_matchf_t	rttr_match;		/* function to match node */
	const rtt_alloc_t *rttr_alloc_funcs;	/* allocation functions */
	rtt_stats_t	*rttr_stats;		/* ptr to stats for structure */
};

/*
 * The rtt_colormask_t type has a value with is exclusive-or'd with
 * the color in the low order 16 bits, and a mask and'd with the
 * result in the high order 16 bits.  The macro makes a constant
 * colormask from its component values, while the inline function
 * does it at run time.
 */
#define	RTT_COLOR_SHIFT		16
#define	RTT_COLOR_MASK		0xffff

#define	RTT_COLORMASK(mask, value) \
	(((mask) << RTT_COLOR_SHIFT) | ((value) & RTT_COLOR_MASK))

static inline rtt_colormask_t
rtt_colormask (rtt_colormask_t mask, rtt_colormask_t value)
{

	return (RTT_COLORMASK(mask, value));
}

/*
 * Magic constants for various things
 */
#define	RTTREE_PTR_NONE		0		/* no key pointer */
#define	RTTREE_COLOR_NONE	0		/* no color */
#define	RTTREE_CMASK_NONE	0		/* no color mask */

/*
 * Return codes from various functions.  The positive ones are
 * things that might normally happen, the negative ones are
 * caller programming errors.
 */
#define	RTTREE_NOMEMORY		2		/* can't get memory */
#define	RTTREE_DUP		1		/* dup node in tree */
#define	RTTREE_OK		0		/* it's all good */
#define	RTTREE_ERR_LENGTH	(-1)		/* key length too long */
#define	RTTREE_ERR_PARAM	(-2)		/* missing required params */
#define	RTTREE_ERR_NOTFOUND	(-3)		/* node not found in delete */
#define	RTTREE_ERR_KEY_OFFSET	(-4)
#define	RTTREE_ERR_PTR_OFFSET	(-5)
#define	RTTREE_ERR_LUT_SIZE	(-6)
#define	RTTREE_ERR_ROOT_ALIGN	(-7)
#define	RTTREE_ERR_NOT_EMPTY	(-8)
#define	RTTREE_ERR_INVALID	(-9)

/*
 * Trivial inline functions here.
 */

/*
 * rttree_key_length()
 *
 * Return the length, in bits, of a node's key
 */
static inline unsigned int
rttree_key_length(const rtnode_t *rn)
{

	return (rn->rttn_len);
}

/*
 * rttree_color()
 *
 * Return the color of a node.
 */
static inline rtt_color_t
rttree_color (const rtnode_t *rn)
{

	return (rn->rttn_color);
}


/*
 * rttree_is_child()
 *
 * Returns true if the node is a child, i.e. if there are no more
 * specific matching nodes in the tree.
 */
static inline bool
rttree_is_child (rtnode_t *rn)
{
	volatile rtt_inode_t *ri = ((volatile rtnode_t *) rn)->rttn_parent;

	return (ri->rtti_bit != rn->rttn_len);
}


/*
 * rttree_dest_next()
 *
 * Given an external node, return the next external node with the
 * same destination.  Access the current node through a volatile
 * pointer to make sure we read it just once.
 */
static inline rtnode_t *
rttree_dest_next (rtnode_t *rn)
{
	volatile rtnode_t *v_rn = rn;

	return ((rtnode_t *) v_rn->rttn_union.rttu_next);
}


/*
 * rttree_dest_next_v()
 *
 * Same as rttree_dest_next().  Works the same.
 */
static inline rtnode_t *
rttree_dest_next_v (rtnode_t *rn)
{

	return (rttree_dest_next(rn));
}


/*
 * rttree_parent()
 *
 * Given an external node, return an external node with a shorter, but
 * matching, prefix.  This only works right in non-volatile trees since
 * it doesn't handle transient states which can occur during an rttree
 * modification.
 */
static inline rtnode_t *
rttree_parent (rtnode_t *rn)
{
	rtt_inode_t *ri;

	ri = rn->rttn_parent;
	if (rn->rttn_len != ri->rtti_bit &&
	    (rn = (rtnode_t *) ri->rtti_attached) != NULL) {
		return (rn);
	}
	ri = ri->rtti_aparent;
	return ((rtnode_t *) ri->rtti_attached);
}


/*
 * rttree_parent_v()
 *
 * Version of the above to work in a volatile tree.  The inode
 * rtti_aparent pointer can point at a node without an attached
 * external in this case.
 */
static inline rtnode_t *
rttree_parent_v (rtnode_t *rn)
{
	volatile rtt_inode_t *ri;

	ri = ((volatile rtnode_t *) rn)->rttn_parent;
	if (((volatile rtnode_t *) rn)->rttn_len != ri->rtti_bit &&
	    (rn = (rtnode_t *) ri->rtti_attached) != NULL) {
		return (rn);
	}
	do {
		ri = ri->rtti_aparent;
		if (ri == NULL) {
			return (NULL);
		}
		rn = (rtnode_t *) ri->rtti_attached;
	} while (rn == NULL);

	return (rn);
}


/*
 * References to the two built-in optimization schedules.
 */
extern const rtt_opt_t rttree_opt_timid[];
extern const rtt_opt_t rttree_opt_aggressive[];

/*
 * External functions
 */
int	rttree_add(rttree_t *, rtnode_t *, unsigned int);
int	rttree_add_c(rttree_t *, rtnode_t *, unsigned int, rtt_color_t);
int	rttree_change(rttree_t *, rtnode_t *, rtnode_t *);
int	rttree_change_c(rttree_t *, rtnode_t *, rtt_color_t, rtnode_t *);
int	rttree_delete(rttree_t *, rtnode_t *);

rtnode_t *rttree_destroy(rttree_t *);

rtnode_t *rttree_find(rttree_t *, const uint32_t *, unsigned int);

rtnode_t *rttree_getnext(rttree_t *, const uint32_t *, unsigned int, bool);
rtnode_t *rttree_getnext_v(rttree_t *, const uint32_t *, unsigned int, bool);

rtnode_t *rttree_getprev(rttree_t *, const uint32_t *, unsigned int, bool);
rtnode_t *rttree_getprev_v(rttree_t *, const uint32_t *, unsigned int, bool);

int	rttree_init(rttree_t *, size_t, size_t, rtt_size_t, rtt_colormask_t,
		    const rtt_alloc_t *, const rtt_cmpfuncs_t *, rtt_stats_t *);

rtnode_t *rttree_lookup(rttree_t *, const uint32_t *);
rtnode_t *rttree_lookup_c(rttree_t *, const uint32_t *, rtt_colormask_t,
			  rtt_colormask_t);
rtnode_t *rttree_lookup_cx(rttree_t *, const uint32_t *, rtt_colormask_t,
			   rtt_colormask_t, const void *);
rtnode_t *rttree_lookup_x(rttree_t *, const uint32_t *, const void *);

rtnode_t *rttree_lookup_v(rttree_t *, const uint32_t *);
rtnode_t *rttree_lookup_c_v(rttree_t *, const uint32_t *, rtt_colormask_t,
			    rtt_colormask_t);
rtnode_t *rttree_lookup_cx_v(rttree_t *, const uint32_t *, rtt_colormask_t,
			     rtt_colormask_t, const void *);
rtnode_t *rttree_lookup_x_v(rttree_t *, const uint32_t *, const void *);

rtnode_t *rttree_next(rttree_t *, rtnode_t *);
rtnode_t *rttree_next_c(rttree_t *, rtnode_t *, rtt_colormask_t,
			rtt_colormask_t);
rtnode_t *rttree_next_cx(rttree_t *, rtnode_t *, rtt_colormask_t,
			 rtt_colormask_t, const void *);
rtnode_t *rttree_next_x(rttree_t *, rtnode_t *, const void *);

rtnode_t *rttree_next_v(rttree_t *, rtnode_t *);
rtnode_t *rttree_next_c_v(rttree_t *, rtnode_t *, rtt_colormask_t,
			  rtt_colormask_t);
rtnode_t *rttree_next_cx_v(rttree_t *, rtnode_t *, rtt_colormask_t,
			   rtt_colormask_t, const void *);
rtnode_t *rttree_next_x_v(rttree_t *, rtnode_t *, const void *);

rtnode_t *rttree_plookup(rttree_t *, const uint32_t *, unsigned int);
rtnode_t *rttree_plookup_c(rttree_t *, const uint32_t *, unsigned int,
			   rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_plookup_cx(rttree_t *, const uint32_t *, unsigned int,
			    rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_plookup_x(rttree_t *, const uint32_t *, unsigned int,
			   const void *);

rtnode_t *rttree_plookup_v(rttree_t *, const uint32_t *, unsigned int);
rtnode_t *rttree_plookup_c_v(rttree_t *, const uint32_t *, unsigned int,
			     rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_plookup_cx_v(rttree_t *, const uint32_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_plookup_x_v(rttree_t *, const uint32_t *, unsigned int,
			     const void *);

rtnode_t *rttree_prev(rttree_t *, rtnode_t *);
rtnode_t *rttree_prev_c(rttree_t *, rtnode_t *, rtt_colormask_t,
			rtt_colormask_t);
rtnode_t *rttree_prev_cx(rttree_t *, rtnode_t *, rtt_colormask_t,
			 rtt_colormask_t, const void *);
rtnode_t *rttree_prev_x(rttree_t *, rtnode_t *, const void *);

rtnode_t *rttree_prev_v(rttree_t *, rtnode_t *);
rtnode_t *rttree_prev_c_v(rttree_t *, rtnode_t *, rtt_colormask_t,
			  rtt_colormask_t);
rtnode_t *rttree_prev_cx_v(rttree_t *, rtnode_t *, rtt_colormask_t,
			   rtt_colormask_t, const void *);
rtnode_t *rttree_prev_x_v(rttree_t *, rtnode_t *, const void *);

rtnode_t *rttree_prune_next(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_prune_next_c(rttree_t *, rtnode_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_prune_next_cx(rttree_t *, rtnode_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_prune_next_x(rttree_t *, rtnode_t *, unsigned int,
			      const void *);

rtnode_t *rttree_prune_next_v(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_prune_next_c_v(rttree_t *, rtnode_t *, unsigned int,
			        rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_prune_next_cx_v(rttree_t *, rtnode_t *, unsigned int,
			        rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_prune_next_x_v(rttree_t *, rtnode_t *, unsigned int,
			        const void *);

rtnode_t *rttree_prune_prev(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_prune_prev_c(rttree_t *, rtnode_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_prune_prev_cx(rttree_t *, rtnode_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_prune_prev_x(rttree_t *, rtnode_t *, unsigned int,
			      const void *);

rtnode_t *rttree_prune_prev_v(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_prune_prev_c_v(rttree_t *, rtnode_t *, unsigned int,
				rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_prune_prev_cx_v(rttree_t *, rtnode_t *, unsigned int,
				rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_prune_prev_x_v(rttree_t *, rtnode_t *, unsigned int,
				const void *);

int	rttree_release(rttree_t *);
void	rttree_stats_init(rtt_stats_t *);

rtnode_t *rttree_sub_first(rttree_t *, const uint32_t *, unsigned int);
rtnode_t *rttree_sub_first_c(rttree_t *, const uint32_t *, unsigned int,
			     rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_first_cx(rttree_t *, const uint32_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_first_x(rttree_t *, const uint32_t *, unsigned int,
			     const void *);

rtnode_t *rttree_sub_first_v(rttree_t *, const uint32_t *, unsigned int);
rtnode_t *rttree_sub_first_c_v(rttree_t *, const uint32_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_first_cx_v(rttree_t *, const uint32_t *, unsigned int,
				rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_first_x_v(rttree_t *, const uint32_t *, unsigned int,
			       const void *);

rtnode_t *rttree_sub_last(rttree_t *, const uint32_t *, unsigned int);
rtnode_t *rttree_sub_last_c(rttree_t *, const uint32_t *, unsigned int,
			    rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_last_cx(rttree_t *, const uint32_t *, unsigned int,
			     rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_last_x(rttree_t *, const uint32_t *, unsigned int,
			    const void *);

rtnode_t *rttree_sub_last_v(rttree_t *, const uint32_t *, unsigned int);
rtnode_t *rttree_sub_last_c_v(rttree_t *, const uint32_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_last_cx_v(rttree_t *, const uint32_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_last_x_v(rttree_t *, const uint32_t *, unsigned int,
			      const void *);

rtnode_t *rttree_sub_next(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_sub_next_c(rttree_t *, rtnode_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_next_cx(rttree_t *, rtnode_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_next_x(rttree_t *, rtnode_t *, unsigned int, const void *);

rtnode_t *rttree_sub_next_v(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_sub_next_c_v(rttree_t *, rtnode_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_next_cx_v(rttree_t *, rtnode_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_next_x_v(rttree_t *, rtnode_t *, unsigned int,
			      const void *);

rtnode_t *rttree_sub_prev(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_sub_prev_c(rttree_t *, rtnode_t *, unsigned int,
			    rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_prev_cx(rttree_t *, rtnode_t *, unsigned int,
			     rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_prev_x(rttree_t *, rtnode_t *, unsigned int, const void *);

rtnode_t *rttree_sub_prev_v(rttree_t *, rtnode_t *, unsigned int);
rtnode_t *rttree_sub_prev_c_v(rttree_t *, rtnode_t *, unsigned int,
			      rtt_colormask_t, rtt_colormask_t);
rtnode_t *rttree_sub_prev_cx_v(rttree_t *, rtnode_t *, unsigned int,
			       rtt_colormask_t, rtt_colormask_t, const void *);
rtnode_t *rttree_sub_prev_x_v(rttree_t *, rtnode_t *, unsigned int,
			      const void *);

int	rttree_tune(rttree_t *, const rtt_opt_t *);

#endif	/* RTTREE_H_ */
