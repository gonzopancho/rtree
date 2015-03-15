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
 * rttree_add.c
 *
 * Functions which add an external node to an rttree.  These
 * include rttree_add() and rttree_add_color().
 */
#include <rttree_update.h>

/*
 * rn_add()
 *
 * Add an external node to the tree.
 */
static int
rn_add (rttree_t *rr, rtnode_t *add_rn, unsigned int prefix_length,
	rtt_color_t color)
{
	rtnode_t *rn, *old_rn;
	rtt_ibit_t plen, diff_bit, bit;
	rtt_inode_t *ri, *up_ri;
	const uint32_t *kp;
	uint32_t k;
	rtt_ibit_t which_k;
	rtt_ptr_t r_ptr;

	/*
	 * Length check the prefix length.  If it is okay write it
	 * and the color into the node.
	 */
	if (prefix_length > RN_MAX_PLENGTH) {
		return (RTTREE_ERR_LENGTH);
	}
	add_rn->rttn_len = (rtt_bit_t) (plen = prefix_length);
	add_rn->rttn_color = color;

	/*
	 * Fetch the key and prefix length from the node we're
	 * adding, then search for a match and find how we differ.
	 * We do the search
	 */
	ri = &rr->rttr_root;
	kp = rn_key(rr, add_rn);

	/*
	 * Test our way down using the LUTs until we run out of
	 * prefix bits.
	 */
	r_ptr = RP_NULL;
	RN_KEYLUT_INIT(RN_NOBIT, kp, k, which_k);
	while ((bit = ri->rtti_bit) < plen) {
		uint32_t l_word;
		rtt_lut_t lut;

		RN_KEYLUT(l_word, bit, kp, k, which_k);
		lut = ri->rtti_lut;
		r_ptr = lut_ptr(lut)[l_word >> lut_ishift(lut)];
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rp_to_int(r_ptr);
	}
	RN_KEYLUT_TO_KEYBIT(which_k);
	if (bit < plen) {
		rtt_ibit_t max_bit;
		rtt_inode_t *l_ri;

		/*
		 * Maybe didn't make it to our prefix length.  If
		 * we got a child external from the LUTs it is
		 * our best match.  Make sure to set `ri' to
		 * his parent inode, though, since the value we
		 * have now is the one where the LUT is.  If
		 * he's attached he's okay as long as he's at least
		 * the length of the new prefix, otherwise we'll
		 * need to search for a longer guy to match against.
		 */
		l_ri = ri;
		if (r_ptr != RP_NULL) {
			ri = rp_to_ext(r_ptr)->rttn_parent;
			if (ri->rtti_attached != r_ptr ||
			    ri->rtti_bit >= plen) {
				goto got_one;
			}
		}

		/*
		 * Okay, at this point we know we got a NULL or
		 * attached out of the last LUT lookup.  That means
		 * that there is a difference between our key and
		 * the tree contents in the bits covered by the LUT.
		 * We must do a bit-by-bit search against our key to
		 * that depth to make sure we capture that.
		 */
		max_bit = bit + lut_isize(l_ri->rtti_lut) + 1;
		if (max_bit > plen) {
			max_bit = plen;
		}
		while ((bit = ri->rtti_bit) < max_bit) {
			rtt_ibit_t dir;

			RN_KEYBIT(dir, bit, kp, k, which_k);
			r_ptr = ri->rtti_child[dir];
			if (rp_is_ext(r_ptr)) {
				if (r_ptr == RP_NULL) {
					r_ptr = ri->rtti_attached;
				}
				goto got_one;
			}
			ri = rp_to_int(r_ptr);
		}
	}


	/*
	 * Past the end of his prefix, or at least the place where
	 * we know there's a difference.  Return the first external
	 * we find, moving to the left if we don't.
	 */
	for (;;) {
		r_ptr = ri->rtti_attached;
		if (r_ptr != RP_NULL) {
			break;
		}
		r_ptr = ri->rtti_left;
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rp_to_int(r_ptr);
	}

	/*
	 * We end up here when we find an external.  Get its pointer.
	 * If it is non-NULL, compare its key to the key we're adding.
	 */
got_one:
	rn = rp_to_ext(r_ptr);
	if (rn) {
		bit = rn->rttn_len;
		diff_bit = rn_cmp(kp, rn_key(rr, rn),
				  ((plen < bit) ? plen : bit));

		/*
		 * Find a parent inode testing a bit less than or
		 * equal to diff_bit.  What we add will go below.
		 */
		up_ri = ri;
		while (up_ri->rtti_bit > diff_bit) {
			up_ri = up_ri->rtti_parent;
		}
	} else {
		/*
		 * Uninitialized pointer in root inode.
		 */
		bit = diff_bit = 0;
		up_ri = &rr->rttr_root;
	}


	old_rn = NULL;
	if (rn == NULL) {
		/*
		 * This guy belongs in an uninitialized pointer
		 * in the root inode.  He must be the only one
		 * for his destination (which could be a default
		 * node).  Just insert him where he belongs.
		 */
		add_rn->rttn_next = RP_NULL;
		add_rn->rttn_parent = ri;
		RTT_WMB();	/* make sure the above is done */
		if (plen == 0) {
			/*
			 * New default route.  Point the attached
			 * pointer at him.
			 */

			/* Structure change starts here */
			ri->rtti_attached = rp_from_ext(add_rn);
			/* Structure change ends here */
		} else {
			/*
			 * Child.  Figure out which side he belongs
			 * in and write him there.
			 */
			RN_KEYBIT(bit, 0, kp, k, which_k);

			/* Structure change starts here */
			ri->rtti_child[bit] = rp_from_ext(add_rn);
			/* Structure change ends here */
		}
	} else if (diff_bit == plen && diff_bit == bit) {
		rtnode_t *prev_rn;
		int res;

		/*
		 * This has the same prefix as something
		 * already in the tree.  If it goes after
		 * that something we can just add it with
		 * no effect on the auxilliary structures,
		 * otherwise we'll need to fix those up.
		 */
		prev_rn = NULL;
		do {
			res = rn_cmp_ext(rr, add_rn, rn);
			if (res >= RTT_CMP_EQ) {
				break;
			}
			prev_rn = rn;
			rn = rp_to_ext(rn->rttn_next);
		} while (rn);

		/*
		 * If these are the same return the error.
		 */
		if (res == RTT_CMP_EQ) {
			return (RTTREE_DUP);
		}

		/*
		 * Fill in the external node.
		 */
		add_rn->rttn_next = rp_from_ext(rn);
		add_rn->rttn_parent = ri;
		RTT_WMB();

		/*
		 * If he's not first in the chain he won't
		 * effect much, so just slip him into place.
		 */
		if (prev_rn) {
			/* Structure change starts here */
			prev_rn->rttn_next = rp_from_ext(add_rn);
			/* Structure change ends here */
			add_rn = NULL;		/* Note no change for below */
		} else {
		
			/*
			 * Okay, work on modifying the tree.  This
			 * is the first thing we've done which might
			 * effect the route lookup.  Replace the old
			 * guy with the new in the child pointers.
			 */
			/* Structure change starts here */
			if (ri->rtti_attached == rp_from_ext(rn)) {
				ri->rtti_attached = rp_from_ext(add_rn);
			} else if (ri->rtti_left == rp_from_ext(rn)) {
				ri->rtti_left = rp_from_ext(add_rn);
			} else if (ri->rtti_right == rp_from_ext(rn)) {
				ri->rtti_right = rp_from_ext(add_rn);
			}
			/* Structure change ends here */

			/*
			 * Note that we replaced an existing node.
			 */
			old_rn = rn;
		}
	} else if (diff_bit == bit && diff_bit == ri->rtti_bit) {
		/*
		 * He's longer, but a prefix match for an attached guy.
		 * We'll add him as a child of this inode.  Figure out
		 * which side he's on (the index ends up in bit).
		 */
		RN_KEYBIT(bit, bit, kp, k, which_k);

		/*
		 * Fill in the node.  Make sure that is visible.
		 */
		add_rn->rttn_next = RP_NULL;
		add_rn->rttn_parent = ri;
		RTT_WMB();

		/*
		 * Okay, now do the deed.
		 */
		/* Structure change starts here */
		ri->rtti_child[bit] = rp_from_ext(add_rn);
		/* Structure change ends here */
	} else if (diff_bit == plen && up_ri->rtti_bit == plen) {
		/*
		 * He was a shorter prefix match for the route we
		 * found and needs to be attached to up_ri (which
		 * won't have an attached route or we would have
		 * found it).  Arrange for that.
		 */
		add_rn->rttn_next = RP_NULL;
		add_rn->rttn_parent = up_ri;
		RTT_WMB();

		/*
		 * Do the deed now.  He'll go into the attached
		 * pointer only.
		 */
		/* Structure change starts here */
		up_ri->rtti_attached = rp_from_ext(add_rn);
		/* Structure change ends here */
	} else {
		rtt_ptr_t ptr;
		rtt_ibit_t dn_dir, up_dir;

		/*
		 * All the remaining cases require a new inode, which
		 * will be a child of up_ri.  Get it and fill it in.
		 */
		ri = ri_alloc(rr);
		if (ri == NULL) {
			return (RTTREE_NOMEMORY);
		}

		add_rn->rttn_next = RP_NULL;
		add_rn->rttn_parent = ri;

		ri->rtti_bit = (rtt_bit_t) diff_bit;
		ri->rtti_parent = up_ri;

		/*
		 * The attached parent is either up_ri or
		 * up_ri->rtti_aparent.  The comparison of the
		 * latter against NULL catches the case where
		 * up_ri is the root inode, in which case he's
		 * an attached parent whether he has an attached
		 * node or not.
		 */
		if (up_ri->rtti_attached != RP_NULL ||
		    up_ri->rtti_aparent == NULL) {
			ri->rtti_aparent = up_ri;
		} else {
			ri->rtti_aparent = up_ri->rtti_aparent;
		}

		/*
		 * Get a copy of the pointer from up_ri that is in
		 * the spot that will point at us.  That will be
		 * our inode's child when we're done.
		 */
		bit = up_ri->rtti_bit;
		RN_KEYBIT(up_dir, bit, kp, k, which_k);
		ptr = up_ri->rtti_child[up_dir];

		/*
		 * Figure out where our new route will live in
		 * the downward direction.  We can fill in the
		 * child pointers knowing that.
		 */
		if (plen > diff_bit) {
			/*
			 * We can figure this out from the new node's
			 * key, which is maximally convenient.
			 */
			RN_KEYBIT(dn_dir, diff_bit, kp, k, which_k);
			ri->rtti_child[dn_dir] = rp_from_ext(add_rn);
			if (diff_bit == rn->rttn_len) {
				/*
				 * Old guy is attached to this node
				 * (and ptr == rp_from_ext(rn)).  NULL
				 * the remaining child pointer.
				 */
				ri->rtti_attached = rp_from_ext(rn);
				ri->rtti_child[RN_NOT(dn_dir)] = RP_NULL;
			} else {
				/*
				 * Old guy is down from this node.  The
				 * attached pointer is NULL.
				 */
				ri->rtti_attached = RP_NULL;
				ri->rtti_child[RN_NOT(dn_dir)] = ptr;
			}
		} else {
			/*
			 * The guy we're adding will be attached and
			 * doesn't extend to this bit.  We'll need
			 * to test the bit in the guy we compared to
			 * ('rn') instead and take the complement of
			 * that.
			 */
			ri->rtti_attached = rp_from_ext(add_rn);
			dn_dir = rn_key_bit(rn_key(rr, rn), diff_bit);
			ri->rtti_child[dn_dir] = ptr;
			ri->rtti_child[RN_NOT(dn_dir)] = RP_NULL;
		}


		/*
		 * Everything is filled in in the new inode and the
		 * added route; we need to make this visible before
		 * changing the structure.  Two jobs remain; we need
		 * to rewrite the parent pointer(s) in whatever `ptr'
		 * points at, and we need to rewrite the down pointer
		 * in `up_ri' to point at the new inode.  Since we
		 * are changing structure already in the tree it is
		 * possible concurrent lookups will see this, and this
		 * is not an atomic change with respect to those lookups,
		 * though it is clear that lookups seeing an incomplete
		 * change will still either get the old result (i.e.
		 * won't find add_rn) or the new result (will find add_rn)
		 * and nothing else.
		 *
		 * We still need to pick one of these to do first,
		 * though.  We'll pick the adjustment of the parent
		 * pointers to do first since they are not used by
		 * most types of lookups.
		 */
		RTT_WMB();

		/* Structure change starts here */
		rp_reparent(ptr, ri);
		RTT_WMB();	/* Make sure this is really done first. */
		up_ri->rtti_child[up_dir] = rp_from_int(ri);
		/* Structure change ends here */
	}

	/*
	 * At this point we've got the new node in the basic tree,
	 * but none of the optimizing data structures (LUTs, aparent
	 * pointers) have been adjusted for it.  Do that now.  Note
	 * that if add_rn is NULL here it is because the change that
	 * was made was invisible but we still need to count it.
	 */
	if (add_rn != NULL) {
		rttree_update_(rr, add_rn, old_rn);
	}

	/*
	 * Count this.  We have a new destination only if add_rn is
	 * non-NULL and old_rn is NULL.
	 */
	if (add_rn != NULL && old_rn == NULL) {
		rr_inc_dests(rr);
	}
	rr_inc_nodes(rr);
	rr_inc_adds(rr);
	return (RTTREE_OK);
}


/*
 * rttree_add_c()
 *
 * Add an external node with the specified color.  Depend on the
 * compiler to recognize the tail call.
 */
int
rttree_add_c (rttree_t *rr, rtnode_t *rn,
	      unsigned int prefix_length, rtt_color_t color)
{

	return (rn_add(rr, rn, prefix_length, color));
}


/*
 * rttree_add()
 *
 * Add an external node.  Use a default color.  Depend on the
 * compiler to recognize the tail call.
 */
int
rttree_add (rttree_t *rr, rtnode_t *rn, unsigned int prefix_length)
{

	return (rn_add(rr, rn, prefix_length, RTTREE_COLOR_NONE));
}
