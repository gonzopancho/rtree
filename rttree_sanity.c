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
 * rttree_sanity.c
 *
 * Provides rttree_sanity_(), a function to check the fundamental soundness
 * of an rttree data structure.  Used for debugging the structure
 * modification functions.
 */
#include <rttree_defs.h>
#include <rttree_sanity.h>

/*
 * This does two passes through the structure.  It scans the basic data
 * structure making sure that parent pointers point at the thing
 * that refers to them as a child, that the keys for same destination
 * nodes are in fact the same, that bits tested and prefix length monotonically
 * increase down the tree and that child external nodes from each side
 * of an inode have keys which differ appropriately.  Having verified
 * the basic data structure it makes a second pass looking at the contents
 * of LUTs, checking that the related inode flags are correctly set and,
 * if appropriate, that the LUT arrangement is optimal given our rules.
 *
 * To ensure the implementation of this checker is as independent of the
 * builder implementation as possible (or, as possible as the same author
 * can manage it), almost everything done here is done via recursion.
 * Since the actual builders avoid this if at all possible, using it
 * exclusively here does everything a whole other way.
 */

/*
 * The following stuff is copied from rttree_update.c.  Please keep it
 * the same.
 */
typedef struct cnt_desc_ {
	uint8_t		cd_idx;		/* index to byte with this count */
	uint8_t		cd_shift;	/* shift to isolate count */
	uint8_t		cd_mask;	/* mask to isolate count */
	uint8_t		cd_inc;		/* value to add to increment count */
} cnt_desc_t;

static const cnt_desc_t cnt_descs[RTT_LUT_SIZES] = {
	{ 0,	0,	0x00,	0x00 },	/* invalid rank */
	{ 4,	4,	0x30,	0x10 },	/* rank 1, max 2 */
	{ 3,	5,	0xe0,	0x20 },	/* rank 2, max 4 */
	{ 4,	0,	0x0f,	0x01 },	/* rank 3, max 8 */
	{ 3,	0,	0x1f,	0x01 },	/* rank 4, max 16 */
	{ 2,	0,	0x3f,	0x01 },	/* rank 5, max 32 */
	{ 1,	0,	0x7f,	0x01 },	/* rank 6, max 64 */
	{ 0,	0,	0xff,	0x01 },	/* rank 7, max 128 */
};

#define	CNT_TARGET	2	/* the count we store the flags in */
#define	RI_DELETED	0x80	/* the `delete' flag; this node to be deleted */
#define	RI_TARGET	0x40	/* the `target' flag: this node changing */

#define	CNT_USELESS	4	/* the count we store `covered' and `lut' */
#define	RI_COVERED	0x80	/* the inode is `covered' by an LUT above */
#define	RI_HAS_LUT	0x40	/* the inode has a non-trivial LUT of its own */
#define	rtti_covered	rtti_counts[CNT_USELESS]
#define	rtti_has_lut	rtti_counts[CNT_USELESS]

#define	rtti_deleted	rtti_counts[CNT_TARGET]

/*
 * Error returns.  If RTTREE_DEBUG is defined we assert(), otherwise
 * we just return an error.
 */
typedef int rserr_t;
#ifndef RTTREE_DEBUG
#define	RTTREE_DEBUG
#endif
#ifdef RTTREE_DEBUG
#include <stdlib.h>
#define	RSERR(i)	abort()
#else
#define	RSERR(i)	return(i)
#endif

/*
 * cnt_get()
 *
 * Given a bit number, get the count out of an inode.
 */
static inline unsigned int
cnt_get (rtt_inode_t *ri, unsigned int i)
{
	const cnt_desc_t *cd;
	unsigned int c;

	if (i == 0) {
		return (0);
	}
	cd = &cnt_descs[i];
	c = ri->rtti_counts[cd->cd_idx];
	c &= cd->cd_mask;
	c >>= cd->cd_shift;
	return (c);
}


/*
 * cnt_extract()
 *
 * Extract cumulative counts from an inode into an easier form to
 * deal with.
 */
static inline void
cnt_extract (rtt_inode_t *ri, uint8_t *cnts)
{
	rtt_ibit_t b, i;
	const cnt_desc_t *cd;
	uint8_t val;
	const uint8_t *ri_cnts;

	b = RN_BIT(ri->rtti_bit);
	ri_cnts = ri->rtti_counts;
	for (i = 0; i < RTT_LUT_SIZES; i++) {
		if (i <= b) {
			cnts[i] = 0;
			continue;
		}
		cd = &cnt_descs[i];
		val = ri_cnts[cd->cd_idx];
		val &= cd->cd_mask;
		val >>= cd->cd_shift;
		cnts[i] = val + cnts[i-1];
	}
}


/*
 * Node count structure.  Used to count nodes in the ranks below us.
 */
typedef struct node_cnt_ {
	uint8_t cnt[RN_NBBY];
} node_cnt_t;

/*
 * Inline which, given a bit number and a node_cnt_t, increments
 * the appropriate count.
 */
static inline void
cnt_inc (node_cnt_t *nc, rtt_ibit_t bit)
{
	nc->cnt[RN_BIT(bit)]++;
}


/*
 * Inline which adds the counts in the second argument to the
 * first.
 */
static inline void
cnt_add (node_cnt_t *to_nc, node_cnt_t *nc)
{
	unsigned int i;

	for (i = 0; i < RN_NBBY; i++) {
		to_nc->cnt[i] += nc->cnt[i];
	}
}

/*
 * Inline to zero a node count.
 */
static inline void
cnt_zero (node_cnt_t *nc)
{
	unsigned int i;

	for (i = 0; i < RN_NBBY; i++) {
		nc->cnt[i] = 0;
	}
}


/*
 * Inline to zero the stats structure.
 */
static inline void
stats_zero (rtt_stats_t *rs)
{
	unsigned int i;

	if (rs) {
		rs->rtts_adds = 0;
		rs->rtts_deletes = 0;
		rs->rtts_changes = 0;
		for (i = 0; i < RTT_LUT_NUM; i++) {
			rs->rtts_luts[i] = 0;
		}
		rs->rtts_inodes = 0;
		rs->rtts_dests = 0;
		rs->rtts_nodes = 0;
	}
}


/*
 * Inline to increment the nodes count in a stats structure.
 */
static inline void
inc_nodes (rtt_stats_t *rs)
{

	if (rs) {
		rs->rtts_nodes++;
	}
}


/*
 * Inline to increment the dests count in a stats structure.
 */
static inline void
inc_dests (rtt_stats_t *rs)
{

	if (rs) {
		rs->rtts_dests++;
	}
}


/*
 * Inline to increment the inodes count in a stats structure.
 */
static inline void
inc_inodes (rtt_stats_t *rs)
{

	if (rs) {
		rs->rtts_inodes++;
	}
}


/*
 * Inline to increment the lut counts in a stats structure.
 */
static inline void
inc_luts (rtt_stats_t *rs, rtt_lut_t lut)
{
	unsigned int i;

	if (rs) {
		i = lut_size(lut);
		if (i < RTT_LUT_NUM) {
			rs->rtts_luts[i]++;
		} else {
			rs->rtts_adds++;
		}
	}
}


/*
 * rs_node()
 *
 * Function to verify a list of externals with the same
 * destination.
 */
static rserr_t
rs_external (rttree_t *rr, rtt_inode_t *ri, rtt_ptr_t ptr, rtt_stats_t *rs,
	     rtnode_t **ret_rn)
{
	rtnode_t *rn, *o_rn;
	const uint32_t *kp, *o_kp;
	rtt_ibit_t dir, d_bit, bit, len, ri_bit;
	rtt_ptr_t r_ptr;
	rtt_colormask_t o_col, col;
	int res;

	if (ptr == RP_NULL) {
		return (0);
	}
	if (!rp_is_ext(ptr)) {
		RSERR(1);
	}
	inc_dests(rs);
	inc_nodes(rs);
	ri_bit = ri->rtti_bit;
	rn = rp_to_ext(ptr);
	len = rn->rttn_len;
	kp = rn_key(rr, rn);
	if (ptr == ri->rtti_attached) {
		if (len != ri_bit) {
			RSERR(2);
		}
		dir = 2;
	} else {
		if (len <= ri_bit) {
			RSERR(3);
		}
		if (ptr == ri->rtti_child[RTT_LEFT]) {
			dir = RTT_LEFT;
		} else if (ptr == ri->rtti_child[RTT_RIGHT]) {
			dir = RTT_RIGHT;
		} else {
			RSERR(4);
		}
		bit = rn_key_bit(kp, ri_bit);
		if (bit != dir) {
			RSERR(5);
		}
	}
	if (ret_rn) {
		*ret_rn = rn;
	}
	if (rn->rttn_parent != ri) {
		RSERR(6);
	}
	r_ptr = rn->rttn_next;
	if (r_ptr == RP_NULL) {
		return (0);
	}
	col = rn->rttn_color;
	col ^= rr->rttr_color_mask;
	col &= (rr->rttr_color_mask >> 16);

	while (r_ptr != RP_NULL) {
		if (!rp_is_ext(r_ptr)) {
			RSERR(7);
		}
		o_rn = rn;
		o_kp = kp;
		o_col = col;
		rn = rp_to_ext(r_ptr);
		kp = rn_key(rr, rn);
		inc_nodes(rs);
		if (rn->rttn_len != len) {
			RSERR(8);
		}
		if (rn->rttn_parent != ri) {
			RSERR(9);
		}
		col = rn->rttn_color;
		col ^= rr->rttr_color_mask;
		col &= (rr->rttr_color_mask >> 16);

		d_bit = rn_cmp(o_kp, kp, len);
		if (d_bit != len) {
			RSERR(10);
		}

		if (o_col < col) {
			res = RTT_CMP_LT;
		} else if (o_col > col) {
			res = RTT_CMP_GT;
		} else if (rr->rttr_cmp) {
			res = rr->rttr_cmp(rr, o_rn, rn);
		} else {
			res = RTT_CMP_EQ;
		}

		if (res != RTT_CMP_LT) {
			if (res == RTT_CMP_EQ) {
				RSERR(11);
			}
			RSERR(12);
		}
		r_ptr = rn->rttn_next;
	}

	return (0);
}


/*
 * rs_child()
 *
 * Function to process the child pointer of an inode.  Deals with
 * internal nodes itself, calls rs_external() to deal with them.
 */
static rserr_t
rs_child (rttree_t *rr, rtt_inode_t *p_ri, rtt_ptr_t ptr, rtt_stats_t *rs,
	  node_cnt_t *p_nc, rtnode_t **ret_rn)
{
	rtnode_t *a_rn, *l_rn, *r_rn, *c_rn;
	const uint32_t *kp0, *kp1;
	rtt_inode_t *ri, *a_ri;
	rtt_ptr_t a_ptr, l_ptr, r_ptr;
	rtt_ibit_t bit, p_bit, d_bit, byte;
	node_cnt_t nc;
	unsigned int i, c, p_c;
	rserr_t err;

	/*
	 * If it is an external call rs_external() to handle it.
	 */
	if (rp_is_ext(ptr)) {
		return (rs_external(rr, p_ri, ptr, rs, ret_rn));
	}

	/*
	 * Fetch the inode pointer.  Increment the inode count.
	 * Make sure our three pointers are distinct.
	 */
	inc_inodes(rs);
	ri = rp_to_int(ptr);
	bit = ri->rtti_bit;
	p_bit = p_ri->rtti_bit;
	if (bit <= p_bit) {
		RSERR(13);
	}
	l_ptr = ri->rtti_left;
	r_ptr = ri->rtti_right;
	a_ptr = ri->rtti_attached;
	if (l_ptr == r_ptr || r_ptr == a_ptr || a_ptr == l_ptr) {
		RSERR(14);
	}
	if (ri->rtti_parent != p_ri) {
		RSERR(15);
	}
	if (p_bit == 0 || p_ri->rtti_attached != RP_NULL) {
		a_ri = p_ri;
	} else {
		a_ri = p_ri->rtti_aparent;
	}
	if (a_ri != ri->rtti_aparent) {
		RSERR(16);
	}

	/*
	 * Zero our counts.
	 */
	cnt_zero(&nc);

	/*
	 * If we appear to have an attached guy test him.
	 */
	a_rn = l_rn = r_rn = c_rn = NULL;
	if (a_ptr != RP_NULL) {
		err = rs_external(rr, ri, a_ptr, rs, &a_rn);
		if (err != 0) {
			return (err);
		}
	}

	/*
	 * Now the children.
	 */
	kp0 = kp1 = NULL;
	if (l_ptr != RP_NULL) {
		err = rs_child(rr, ri, l_ptr, rs, &nc, &l_rn);
		if (err != 0) {
			return (err);
		}
		c_rn = l_rn;
		kp0 = rn_key(rr, l_rn);
		if (rn_key_bit(kp0, bit) != RTT_LEFT) {
			RSERR(17);
		}
	}
	if (r_ptr != RP_NULL) {
		err = rs_child(rr, ri, r_ptr, rs, &nc, &r_rn);
		if (err != 0) {
			return (err);
		}
		kp1 = rn_key(rr, r_rn);
		if (rn_key_bit(kp1, bit) != RTT_RIGHT) {
			RSERR(18);
		}
		if (c_rn == NULL) {
			c_rn = r_rn;
			kp0 = kp1;
		}
	}
	if (ret_rn) {
		*ret_rn = c_rn;
	}

	/*
	 * If we have two children, compare them beyond the length
	 * of bit.  They should differ at bit.
	 */
	if (l_rn != NULL && r_rn != NULL) {
		d_bit = rn_cmp(kp0, kp1, bit + 1);
		if (d_bit != bit) {
			RSERR(19);
		}
	}

	/*
	 * Make sure the attached prefix is sane with respect to
	 * a child prefix.
	 */
	if (a_rn != NULL) {
		kp1 = rn_key(rr, a_rn);
		d_bit = rn_cmp(kp0, kp1, bit);
		if (d_bit != bit) {
			RSERR(20);
		}
	}

	/*
	 * So far so fine.  Fetch our byte from the child's key based
	 * on our parent's bit.  Make sure the inode byte is correct.
	 */
	byte = rn_key_byte(kp0, p_bit);
	if (rn_same_byte(p_bit, bit)) {
		byte &= 0xffu << RN_BIT(-bit);
	}
	if (byte != ri->rtti_byte) {
		RSERR(21);
	}

	/*
	 * Compare the child count to the inode's counts.  Add
	 * it to the parent's counts, along with an increment for
	 * our own node.
	 */
	for (i = 1; i < RN_NBBY; i++) {
		p_c = cnt_get(ri, i);
		c = nc.cnt[i];
		if (p_c != c) {
			RSERR(22);
		}
	}
	if (p_nc && rn_same_byte(p_bit, bit)) {
		cnt_add(p_nc, &nc);
		cnt_inc(p_nc, bit);
	}

	/*
	 * We count LUTs but leave the rest for rs_lut_check().  We
	 * do, however, make sure that the deleted and target bits
	 * aren't set at this point.
	 */
	inc_luts(rs, ri->rtti_lut);
	if ((ri->rtti_deleted & (RI_DELETED|RI_TARGET)) != 0) {
		RSERR(23);
	}

	/*
	 * We've looked at everything.  All seems sane.
	 */
	return (0);
}


/*
 * Forward declaration.
 */
static rserr_t rs_lut_check(rttree_t *, rtt_ptr_t, unsigned int *);


/*
 * rs_lut_verify()
 *
 * Verify the contents of a segment of an LUT.  The node in the call
 * is either mentioned in the LUT or is covered by it.
 */
static rserr_t
rs_lut_verify (rttree_t *rr, rtt_ptr_t ptr, rtt_ptr_t def_ptr, rtt_ptr_t *lp,
	       unsigned int *ip, rtt_ibit_t min_bit, rtt_ibit_t max_bit,
	       unsigned int *nopt_p)
{
	unsigned int i, i_end, n, byte;
	rtt_ibit_t bit, p_bit;
	rtnode_t *rn;
	rtt_inode_t *ri, *p_ri;
	const uint32_t *kp;
	rtt_ptr_t c_ptr;
	rserr_t err;

	/*
	 * If it is an external its pointer will be present in the LUT.
	 */
	i = *ip;
	if (rp_is_ext(ptr)) {
		rn = rp_to_ext(ptr);
		bit = rn->rttn_len;
		p_ri = rn->rttn_parent;
		p_bit = p_ri->rtti_bit;
		kp = rn_key(rr, rn);
		byte = rn_key_byte(kp, p_bit);
		byte &= 0xff >> RN_BIT(min_bit);
		if (bit > max_bit) {
			n = 1;
		} else {
			byte &= 0xffu << RN_BIT(-bit);
			n = 2u << (max_bit - bit);
		}
		byte >>= RN_BIT(~max_bit);

		/*
		 * While you might not have followed that, byte
		 * is now the index in the LUT where our pointer
		 * starts, and n is the number of our pointers we
		 * should find.  Make sure it has the default pointer
		 * up to the byte index, and that n of our pointers
		 * follow.
		 */
		if (byte < i) {
			RSERR(24);
		}
		while (i < byte) {
			if (lp[i++] != def_ptr) {
				RSERR(25);
			}
		}

		while (n) {
			if (lp[i++] != ptr) {
				RSERR(26);
			}
			n--;
		}

		/*
		 * That's all.  Record how far we advanced in the LUT
		 * and return no error.
		 */
		*ip = i;
		return (0);
	}

	/*
	 * Here we have an internal node.  The first thing to do is
	 * verify that the LUT points at the default pointer up to his
	 * left edge.  We can trust his byte since it was checked on
	 * an earlier pass.
	 */
	ri = rp_to_int(ptr);
	byte = ri->rtti_byte;
	byte &= 0xff >> RN_BIT(min_bit);
	byte >>= RN_BIT(~max_bit);
	if (byte < i) {
		RSERR(27);
	}
	while (i < byte) {
		if (lp[i++] != def_ptr) {
			RSERR(28);
		}
	}
	bit = ri->rtti_bit;

	/*
	 * So far so good.  If this inode is uncovered by the LUT
	 * it will appear in the LUT just once, in the next location.
	 * It should not have the covered bit set, but it may have
	 * its own LUT which requires checking.
	 */
	if (bit > max_bit) {
		if (lp[i++] != ptr) {
			RSERR(29);
		}
		*ip = i;
		return (rs_lut_check(rr, ptr, nopt_p));
	}


	/*
	 * What we're left with is an inode covered by this LUT.
	 * Make sure the covered bit is set and that the has_lut
	 * bit is not.   Also compute how much of the LUT this
	 * node should cover.
	 */
	i_end = i + (2u << (max_bit - bit));
	*ip = i;
	if ((ri->rtti_covered & RI_COVERED) == 0) {
		RSERR(30);
	}
	if ((ri->rtti_has_lut & RI_HAS_LUT) != 0) {
		RSERR(31);
	}
	if (ri->rtti_lut != lut_make(ri->rtti_child, RTT_LUT_2)) {
		RSERR(32);
	}

	/*
	 * All seems well.  If we have an attached node it will
	 * become the default pointer for LUT segments below.
	 */
	c_ptr = ri->rtti_attached;
	if (c_ptr != RP_NULL) {
		def_ptr = c_ptr;
	}
	c_ptr = ri->rtti_left;
	if (c_ptr != RP_NULL) {
		err = rs_lut_verify (rr, c_ptr, def_ptr, lp, ip, min_bit,
				     max_bit, nopt_p);
		if (err != 0) {
			return (err);
		}
	}
	c_ptr = ri->rtti_right;
	if (c_ptr != RP_NULL) {
		err = rs_lut_verify (rr, c_ptr, def_ptr, lp, ip, min_bit,
				     max_bit, nopt_p);
		if (err != 0) {
			return (err);
		}
	}

	/*
	 * If we've got here we've checked both sides.  If we didn't
	 * reach the end of the part of the LUT segment rooted at
	 * this inode check that the default pointer fills in the
	 * remainder.
	 */
	i = *ip;
	if (i < i_end) {
		do {
			if (lp[i++] != def_ptr) {
				RSERR(33);
			}
		} while (i < i_end);
		*ip = i_end;
	}

	return (0);
}


/*
 * rs_lut_check()
 *
 * Given a pointer to an inode which should not be covered by
 * an LUT (because the LUT above, if any, doesn't extend this far),
 * determine if this node has an LUT which is optimally correct
 * for its situation and, if it does have an LUT, call rs_lut_verify()
 * to ensure that the LUT is filled in properly.  If it does not
 * have an LUT itself it calls itself to check each of its inode
 * children.
 */
static rserr_t
rs_lut_check(rttree_t *rr, rtt_ptr_t ptr, unsigned int *nopt_p)
{
	rtt_inode_t *ri;
	rtt_ibit_t bit, c_bit, max_bit, l_size, w_cnt, c_cnt, min_size;
	rtt_lut_t lut;
	rtt_ptr_t c_ptr;
	rtt_ptr_t *lp;
	unsigned int idx, n;
	const rtt_opt_t *wat_p;
	uint8_t cnts[RTT_LUT_SIZES];
	rserr_t err;

	if (!rp_is_int(ptr)) {
		return (0);	/* probably shouldn't happen */
	}
	ri = rp_to_int(ptr);
	bit = ri->rtti_bit;
	if ((ri->rtti_covered & RI_COVERED) != 0) {
		RSERR(34);
	}
	lut = ri->rtti_lut;
	l_size = lut_isize(lut);
	max_bit = bit + l_size;
	if (!rn_same_byte(bit, max_bit)) {
		RSERR(35);
	}
	if (l_size == 0) {
		lp = ri->rtti_child;
		if (lut != lut_make(lp, RTT_LUT_2)) {
			RSERR(36);
		}
		if ((ri->rtti_has_lut & RI_HAS_LUT) != 0) {
			RSERR(37);
		}
		c_ptr = ri->rtti_left;
		if (rp_is_int(c_ptr)) {
			err = rs_lut_check(rr, c_ptr, nopt_p);
			if (err != 0) {
				return (err);
			}
		}
		c_ptr = ri->rtti_right;
		if (rp_is_int(c_ptr)) {
			err = rs_lut_check(rr, c_ptr, nopt_p);
			if (err != 0) {
				return (err);
			}
		}
	} else {
		lp = lut_ptr(lut);
		if (lp == ri->rtti_child) {
			RSERR(38);
		}
		if ((ri->rtti_has_lut & RI_HAS_LUT) == 0) {
			RSERR(39);
		}
		idx = 0;
		c_ptr = ri->rtti_left;
		if (c_ptr != RP_NULL) {
			err = rs_lut_verify (rr, c_ptr, RP_NULL, lp, &idx,
					     bit, max_bit, nopt_p);
			if (err != 0) {
				return (err);
			}
		}
		c_ptr = ri->rtti_right;
		if (c_ptr != RP_NULL) {
			err = rs_lut_verify (rr, c_ptr, RP_NULL, lp, &idx,
					     bit, max_bit, nopt_p);
			if (err != 0) {
				return (err);
			}
		}
		n = 2u << l_size;
		if (idx > n) {
			RSERR(40);
		}
		while (idx < n) {
			if (lp[idx++] != RP_NULL) {
				RSERR(41);
			}
		}
	}

	/*
	 * Okay, what remains is to check that his LUT is optimum
	 * for his state.  We've checked the inode counts before
	 * so we can trust them.  If he has an LUT start by making
	 * sure that it is above the low water mark (and if it is
	 * the root inode, that it is above the minimum root LUT
	 * size.  We start with the root inode.
	 */
	if (ri == &rr->rttr_root) {
		min_size = lut_isize((rtt_lut_t) rr->rttr_root_lut);
	} else {
		min_size = 0;
	}
	if (l_size < min_size) {
		if (nopt_p) {
			*nopt_p += 1;
		} else {
			RSERR(42);
		}
	}
	wat_p = &rr->rttr_opt[RN_BIT(bit)];
	cnt_extract(ri, cnts);
	if (l_size > min_size) {
		w_cnt = wat_p[RN_BIT(~max_bit)].lo;
		c_cnt = cnts[RN_BIT(max_bit)];
		if (c_cnt < w_cnt) {
			if (nopt_p) {
				*nopt_p += 1;
			} else {
				RSERR(43);
			}
		}
	}

	c_bit = RN_MAXBIT(bit);
	while (c_bit > max_bit) {
		w_cnt = wat_p[RN_BIT(~c_bit)].hi;
		c_cnt = cnts[RN_BIT(c_bit)];
		if (c_cnt >= w_cnt) {
			if (nopt_p) {
				*nopt_p += 1;
			} else {
				RSERR(44);
			}
			break;
		}
		c_bit--;
	}

	/*
	 * All done.
	 */
	return (0);
}


/*
 * rttree_sanity_()
 *
 * This is the entry point for the checker.  It checks the basic
 * sanity of the root inode, calls rs_external() to check the zero-prefix
 * default node (if any) and then calls rs_child() on each of its non-NULL
 * children.  Once that is done it calls rs_lut_check() on the root
 * inode.  If the rttree root structure has stats it will add up its
 * own stats to compare.  If the caller provides a non-NULL nopt_p
 * it will return it with the number of inodes with non-optimal LUTs,
 * otherwise the called functions will return an error if they find one.
 */
int
rttree_sanity_ (rttree_t *rr, unsigned int *nopt_p, rtt_stats_t *his_rs)
{
	rtt_stats_t stats, *rs, *r_rs;
	rtt_inode_t *ri;
	rtt_ibit_t bit;
	rtt_ptr_t a_ptr, l_ptr, r_ptr;
	rtnode_t *a_rn, *l_rn, *r_rn;
	node_cnt_t nc;
	unsigned int i, c, p_c;
	uint32_t sum;
	const uint32_t *kp;
	rserr_t err;

	/*
	 * If he gave us a stats argument we'll add it up.  If he didn't
	 * but the tree root has one we'll use our own.
	 */
	rs = NULL;
	if (his_rs) {
		rs = his_rs;
	} else if (rr->rttr_stats) {
		rs = &stats;
	}
	if (rs) {
		stats_zero(rs);
	}
	if (nopt_p) {
		*nopt_p = 0;
	}

	/*
	 * Check out the root inode.  This repeats work we
	 * did above, but handles the peculiar things about
	 * the root.
	 */
	ri = &rr->rttr_root;
	bit = ri->rtti_bit;
	if (rr->rttr_alloc_funcs == NULL) {
		RSERR(45);
	}
	if (bit != 0) {
		RSERR(46);
	}
	if (ri->rtti_parent != NULL) {
		RSERR(47);
	}
	if (ri->rtti_aparent != NULL) {
		RSERR(48);
	}
	if (ri->rtti_byte != 0) {
		RSERR(49);
	}

	l_ptr = ri->rtti_left;
	r_ptr = ri->rtti_right;
	a_ptr = ri->rtti_attached;
	if (l_ptr != RP_NULL && (l_ptr == r_ptr || l_ptr == a_ptr)) {
		RSERR(50);
	}
	if (r_ptr != RP_NULL && (r_ptr == a_ptr)) {
		RSERR(51);
	}

	/*
	 * Zero our counts.
	 */
	cnt_zero(&nc);

	/*
	 * If we appear to have an attached guy test him.
	 */
	a_rn = l_rn = r_rn = NULL;
	if (a_ptr != RP_NULL) {
		err = rs_external(rr, ri, a_ptr, rs, &a_rn);
		if (err != 0) {
			return (err);
		}
	}

	/*
	 * Now the children.
	 */
	if (l_ptr != RP_NULL) {
		err = rs_child(rr, ri, l_ptr, rs, &nc, &l_rn);
		if (err != 0) {
			return (err);
		}
		kp = rn_key(rr, l_rn);
		if (rn_key_bit(kp, 0) != RTT_LEFT) {
			RSERR(52);
		}
	}
	if (r_ptr != RP_NULL) {
		err = rs_child(rr, ri, r_ptr, rs, &nc, &r_rn);
		if (err != 0) {
			return (err);
		}
		kp = rn_key(rr, r_rn);
		if (rn_key_bit(kp, 0) != RTT_RIGHT) {
			RSERR(53);
		}
	}

	/*
	 * Make sure our counts match the inode's counts.
	 */
	cnt_inc(&nc, bit);
	for (i = 1; i < RN_NBBY; i++) {
		p_c = cnt_get(ri, i);
		c = nc.cnt[i];
		if (p_c != c) {
			RSERR(54);
		}
	}

	/*
	 * We count LUTs but leave the rest for rs_lut_check().  We
	 * do, however, make sure that the deleted and target bits
	 * aren't set at this point.
	 */
	if (lut_size(ri->rtti_lut) != RTT_LUT_2) {
		inc_luts(rs, ri->rtti_lut);
	}
	if ((ri->rtti_deleted & (RI_DELETED|RI_TARGET)) != 0) {
		RSERR(55);
	}

	/*
	 * Call the LUT checker now.
	 */
	err = rs_lut_check(rr, rp_from_int(ri), nopt_p);
	if (err != 0) {
		return (err);
	}

	/*
	 * Check out the stats if he has some.
	 */
	r_rs = rr->rttr_stats;
	if (r_rs) {
		if (r_rs->rtts_nodes != rs->rtts_nodes) {
			RSERR(56);
		}
		if (r_rs->rtts_dests != rs->rtts_dests) {
			RSERR(57);
		}
		if (r_rs->rtts_inodes != rs->rtts_inodes) {
			RSERR(58);
		}
		sum = 0;
		for (i = 0; i < RTT_LUT_NUM; i++) {
			sum += rs->rtts_luts[i];
			if (rs->rtts_luts[i] != r_rs->rtts_luts[i]) {
				RSERR(59);
			}
		}
		sum += (uint32_t) rs->rtts_adds;

		/*
		 * There's an ambiguity over what is counted here,
		 * since 'sum' includes the permanent root inode
		 * while rs->rtts_inodes usually doesn't.  If `sum'
		 * is within 1 we'll let it slide
		 */
		if ((sum - rs->rtts_inodes) > 1) {
			RSERR(60);
		}
	}
	if (rs) {
		rs->rtts_adds = 0;
	}

	return (0);
}
