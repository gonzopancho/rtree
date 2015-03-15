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
 * rttree_update.h
 *
 * Definitions useful to functions which update the data structures.
 */
#ifndef	RTTREE_UPDATE_H_
#define	RTTREE_UPDATE_H_
#include <rttree_defs.h>


/*
 * These are flags passed to rttree_update_() in an inode.  The function
 * expects that the RI_TARGET flag be set in newly-added or about-to-be-deleted
 * inodes, and that the latter also have the RI_DELETED flag set.  The
 * rtti_count[] that these are stored in is CNT_TARGET.
 */
#define	CNT_TARGET	2	/* the count we store the flags in */
#define	RI_DELETED	0x80	/* the `delete' flag; this node to be deleted */
#define	RI_TARGET	0x40	/* the `target' flag: this node changing */

/*
 * RI_HAS_LUT is set in ri->rtti_counts[CNT_HAS_LUT] when the inode
 * has a non-trivial LUT of its own.  This stuff is almost private
 * to rttree_update(), but rttree_init() needs to set it in the root
 * inode.
 */
#define	CNT_HAS_LUT	4
#define	RI_HAS_LUT	0x40	/* the inode has a non-trivial LUT of its own */

/*
 * Macro for ri_alloc() below.
 */
#define	RI_CNT_INIT(i)	(((i) == CNT_TARGET) ? RI_TARGET : 0)

/*
 * Inline to mark an inode as deleted.
 */
static inline void
ri_mark_deleted (rtt_inode_t *ri)
{

	ri->rtti_counts[CNT_TARGET] |= (RI_DELETED|RI_TARGET);
}

/*
 * Routines to increment and decrement tree root stats.  Macroize
 * the check for the stats pointer.
 */
#define	RR_INC_STATS(rr, member) \
	do { \
		rtt_stats_t *Xrs = (rr)->rttr_stats; \
		if (Xrs) { \
			Xrs->member += 1u; \
		} \
	} while (0)

#define RR_DEC_STATS(rr, member) \
	do { \
		rtt_stats_t *Xrs = (rr)->rttr_stats; \
		if (Xrs) { \
			Xrs->member -= 1u; \
		} \
	} while (0)

/*
 * rr_inc_nodes()
 *
 * Increment the count of nodes in the tree.
 */
static inline void
rr_inc_nodes (rttree_t *rr)
{

	RR_INC_STATS(rr, rtts_nodes);
}


/*
 * rr_dec_nodes()
 *
 * Decrement the count of nodes in the tree.
 */
static inline void
rr_dec_nodes (rttree_t *rr)
{

	RR_DEC_STATS(rr, rtts_nodes);
}


/*
 * rr_inc_dests()
 *
 * Increment the count of destinations in the tree
 */
static inline void
rr_inc_dests (rttree_t *rr)
{

	RR_INC_STATS(rr, rtts_dests);
}


/*
 * rr_dec_dests()
 *
 * Decrement the count of destinations in the tree
 */
static inline void
rr_dec_dests (rttree_t *rr)
{

	RR_DEC_STATS(rr, rtts_dests);
}


/*
 * rr_inc_adds()
 *
 * Increment the count of adds done to the tree
 */
static inline void
rr_inc_adds (rttree_t *rr)
{

	RR_INC_STATS(rr, rtts_adds);
}


/*
 * rr_inc_deletes()
 *
 * Increment the count of deletes done to the tree
 */
static inline void
rr_inc_deletes (rttree_t *rr)
{

	RR_INC_STATS(rr, rtts_deletes);
}


/*
 * rr_inc_changes()
 *
 * Increment the count of changes done to the tree
 */
static inline void
rr_inc_changes (rttree_t *rr)
{

	RR_INC_STATS(rr, rtts_changes);
}


/*
 * rr_inc_inodes()
 *
 * Increment the count of inodes in the tree
 */
static inline void
rr_inc_inodes (rttree_t *rr)
{

	RR_INC_STATS(rr, rtts_inodes);
}


/*
 * rr_dec_inodes()
 *
 * Decrement the count of inodes in the tree
 */
static inline void
rr_dec_inodes (rttree_t *rr)
{

	RR_DEC_STATS(rr, rtts_inodes);
}


/*
 * rr_inc_luts()
 *
 * Increment the count of LUTs of the specified size
 */
static inline void
rr_inc_luts (rttree_t *rr, rtt_size_t size)
{
	rtt_stats_t *rs = rr->rttr_stats;

	if (rs) {
		rs->rtts_luts[size]++;
	}
}


/*
 * rr_dec_luts()
 *
 * Decrement the count of LUTs of the specified size
 */
static inline void
rr_dec_luts (rttree_t *rr, rtt_size_t size)
{
	rtt_stats_t *rs = rr->rttr_stats;

	if (rs) {
		rs->rtts_luts[size]--;
	}
}


/*
 * Memory allocation related inlines.  Here so everyone remembers
 * to adjust the statistics.
 */
/*
 * rtt_inode_alloc()
 *
 * Get a new inode structure.
 */
static inline rtt_inode_t *
rtt_inode_alloc (rttree_t *rr)
{
	rtt_inode_t *ri;

	ri = rr->rttr_alloc_funcs->rtta_inode_alloc(rr);
	if (ri) {
		rr_inc_inodes(rr);
	}

	return (ri);
}


/*
 * rtt_inode_free()
 *
 * Free an inode structure which was not in use.
 */
static inline void
rtt_inode_free (rttree_t *rr, rtt_inode_t *ri)
{

	rr->rttr_alloc_funcs->rtta_inode_free(rr, ri);
	rr_dec_inodes(rr);
}


/*
 * rtt_inode_unref()
 *
 * Free an inode structure which was recently in use.
 */
static inline void
rtt_inode_unref (rttree_t *rr, rtt_inode_t *ri)
{

	rr->rttr_alloc_funcs->rtta_inode_unref(rr, ri);
	rr_dec_inodes(rr);
}


/*
 * A NULL value for an LUT.
 */
#define	LUT_NULL	0

/*
 * rtt_lut_alloc()
 *
 * Allocate a new LUT of the specified size.
 */
static inline rtt_lut_t
rtt_lut_alloc (rttree_t *rr, rtt_size_t size)
{
	rtt_ptr_t *lp;

	lp = rr->rttr_alloc_funcs->rtta_lut_alloc(rr, size);
	if (lp) {
		rr_inc_luts(rr, size);
		return (lut_make(lp, size));
	}
	return (LUT_NULL);
}


/*
 * rtt_lut_free()
 *
 * Free a LUT which has not been used.
 */
static inline void
rtt_lut_free (rttree_t *rr, rtt_lut_t lut)
{

	rr->rttr_alloc_funcs->rtta_lut_free(rr, lut_size(lut), lut_ptr(lut));
	rr_dec_luts(rr, lut_size(lut));
}


/*
 * rtt_lut_unref()
 *
 * Free a LUT which was recently in use.
 */
static inline void
rtt_lut_unref (rttree_t *rr, rtt_lut_t lut)
{

	rr->rttr_alloc_funcs->rtta_lut_unref(rr, lut_size(lut), lut_ptr(lut));
	rr_dec_luts(rr, lut_size(lut));
}


/*
 * Change routine specific functions.  These encapsulate some knowledge
 * that is mostly of private use to rttree_update_(), but which needs to
 * be used in the change functions.
 */

/*
 * ri_alloc()
 *
 * Version of rtt_inode_alloc() for use by rttree_add().  This correctly
 * initializes some of the optimization fields which aren't of interest
 * to rttree_add().
 */
static inline rtt_inode_t *
ri_alloc (rttree_t *rr)
{
	rtt_inode_t *ri;

	ri = rtt_inode_alloc(rr);
	if (ri) {
		ri->rtti_lut = lut_make(ri->rtti_child, RTT_LUT_2);
		ri->rtti_byte = 0;
		ri->rtti_counts[0] = RI_CNT_INIT(0);
		ri->rtti_counts[1] = RI_CNT_INIT(1);
		ri->rtti_counts[2] = RI_CNT_INIT(2);
		ri->rtti_counts[3] = RI_CNT_INIT(3);
		ri->rtti_counts[4] = RI_CNT_INIT(4);
	}
	return(ri);
}


/*
 * Functions to (re)write parent pointers in nodes and inodes
 */

/*
 * rn_reparent()
 *
 * Reparent the chain of external nodes onto a new inode.
 */
static inline void rn_reparent(rtnode_t *, rtt_inode_t *) ALWAYS_INLINE;

static inline void
rn_reparent (rtnode_t *rn, rtt_inode_t *p_ri)
{

	while (rn) {
		rn->rttn_parent = p_ri;
		rn = rp_to_ext(rn->rttn_next);
	}
}


/*
 * ri_reparent()
 *
 * Reparent an inode to a new parent inode.
 */
static inline void ri_reparent(rtt_inode_t *, rtt_inode_t *) ALWAYS_INLINE;

static inline void
ri_reparent (rtt_inode_t *ri, rtt_inode_t *p_ri)
{

	ri->rtti_parent = p_ri;
}


/*
 * rp_reparent()
 *
 * Give a rtt_ptr_t pointer, reset its parent pointers
 * to a new inode.
 */
static inline void rp_reparent(rtt_ptr_t, rtt_inode_t *) ALWAYS_INLINE;

static inline void
rp_reparent (rtt_ptr_t ptr, rtt_inode_t *p_ri)
{
	if (rp_is_ext(ptr)) {
		rn_reparent(rp_to_ext(ptr), p_ri);
	} else {
		ri_reparent(rp_to_int(ptr), p_ri);
	}
}


/*
 * rn_check()
 *
 * Performs a search on a key and key length to find a (possible) match
 * in the tree.  This does not do a key comparison with the external
 * it finds, but does ensure that the external has the correct prefix
 * length and is the only possible match in the tree.  Used by
 * rttree_change() and rttree_delete() to ensure that the node
 * he claims to be in the rttree is in fact in that rttree.
 */
static inline rtnode_t *
rn_check(rttree_t *, const uint32_t *, rtt_ibit_t) ALWAYS_INLINE;

static inline rtnode_t *
rn_check (rttree_t *rr, const uint32_t *kp, rtt_ibit_t plen)
{
	rtnode_t *rn;
	rtt_inode_t *ri;
	rtt_ptr_t r_ptr;
	rtt_ibit_t bit, which_k;
	uint32_t k;

	/*
	 * Search down based on the key and using the LUTs
	 * Stop when we find an external.  If the external node is
	 * a child we'll find it in the LUTs.  If it is attached, however,
	 * we may end up at a child prefix instead, so if we get to
	 * an inode with a bit testing beyond the end of the prefix
	 * move back up looking for an attached node.
	 */
	ri = &rr->rttr_root;
	RN_KEYLUT_INIT(RN_NOBIT, kp, k, which_k);
	for (;;) {
		rtt_lut_t lut;
		uint32_t l_word;

		if ((bit = ri->rtti_bit) >= plen) {
			while (ri->rtti_bit > plen) {
				ri = ri->rtti_aparent;
			}
			r_ptr = ri->rtti_attached;
			break;
		}
		RN_KEYLUT(l_word, bit, kp, k, which_k);
		lut = ri->rtti_lut;
		r_ptr = lut_ptr(lut)[l_word >> lut_ishift(lut)];
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rp_to_int(r_ptr);
	}

	/*
	 * At this point r_ptr points at an external.  If the
	 * external is NULL there's no match.  If the prefix
	 * is too long there might be a match attached further
	 * up the tree.  If we don't find something with exactly
	 * the same prefix length there's no match, and if the
	 * parent inode of the node we're deleting is not the
	 * same as the parent inode of the node we found then
	 * the node we're deleting must be in some other tree.
	 */
	rn = rp_to_ext(r_ptr);
	if (rn == NULL) {
		return (NULL);
	}
	ri = rn->rttn_parent;
	if (rn->rttn_len > plen) {
		while (ri->rtti_bit > plen) {
			ri = ri->rtti_aparent;
		}
		if (ri->rtti_bit > 0 && ri->rtti_attached == RP_NULL) {
			ri = ri->rtti_aparent;
		}
		rn = rp_to_ext(ri->rtti_attached);
	}
	if (rn == NULL || rn->rttn_len != plen) {
		return (NULL);
	}
	return (rn);
}


/*
 * External declarations.
 */
void	rttree_update_(rttree_t *, rtnode_t *, rtnode_t *);

#endif	/* RTTREE_UPDATE_H_ */
