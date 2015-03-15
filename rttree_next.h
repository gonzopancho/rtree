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
 * rttree_next.h
 *
 * Provides inline functions to do _next() and _prev() operations in a
 * non-volatile rttree.  These do their walk trusting that the inodes
 * they are looking at won't be changing under them, which makes the
 * basic _next() operation O(1).
 */
#ifndef	RTTREE_NEXT_H_
#define	RTTREE_NEXT_H_
#include <rttree_defs.h>


/*
 * rn_next()
 *
 * Given an external node find the first external node for
 * the next destination in the rttree, in rttree lexical order.
 * first is true if the node is known to be first in his
 * list (this saves looking at the node key), false otherwise.
 *
 * The node given to the function no longer needs to be the first
 * rtnode_t with the key.
 */
static inline rtnode_t *
rn_next(rttree_t *, rtnode_t *, bool, rtt_ibit_t) ALWAYS_INLINE;

static inline rtnode_t *
rn_next (rttree_t *rr, rtnode_t *rn, bool prune, rtt_ibit_t st_bit)
{
	V_ rtt_inode_t *ri;
	V_ rtnode_t *v_rn;
	rtt_ptr_t r_ptr;
	rtt_ibit_t bit, which_k;
	const uint32_t *kp;
	uint32_t k;


	/*
	 * Get his parent inode.  If he's attached go left, if he's
	 * on the left go right, and if he's on the right go back
	 * up the tree looking for an inode where we've not been on
	 * the right yet.  Our goal is to find a pointer to the
	 * first node/inode where we haven't looked yet.
	 *
	 * Initialize us for bit tests.  Doing it here keeps the
	 * compiler from complaining about uninitialized stuff.
	 */
	kp = NULL;
	RN_KEYBIT_INIT(RN_NOBIT, kp, k, which_k);
	if (rn == NULL) {
		/*
		 * A subtree operation with a prefix length greater
		 * than zero makes no sense without an external to provide
		 * the prefix.  A prune is okay, though, it turns
		 * into a regular _next() operation.
		 */
		if (!prune && st_bit > 0) {
			return (NULL);
		}
		r_ptr = rp_from_int(&rr->rttr_root);
		ri = NULL;
		bit = 0;
	} else {
		kp = rn_key(rr, rn);
		v_rn = rn;
		ri = rv_inode(v_rn->rttn_parent);
		bit = ri->rtti_bit;
		if (!prune && bit < st_bit) {
			/*
			 * Either prefix too short to be in subtree
			 * he says it is, or we've already fallen
			 * off the top of the subtree.
			 */
			return (NULL);
		}
		if (prune && bit >= st_bit) {
			/*
			 * The inode is in the subtree we're pruning.
			 * We must unconditionally move back up the
			 * tree.
			 */
			r_ptr = RP_NULL;
		} else if (rn->rttn_len == bit) {
			/*
			 * He's attached.  Go left if we can, right if
			 * we can't.
			 */
			r_ptr = ri->rtti_left;
			if (r_ptr == RP_NULL) {
				r_ptr = ri->rtti_right;
			}
		} else {
			rtt_ibit_t dir;

			/*
			 * See which side he's on.  If he's left go right,
			 * otherwise go up.
			 */
			RN_KEYBIT(dir, bit, kp, k, which_k);
			if (dir == RTT_LEFT) {
				r_ptr = ri->rtti_right;
			} else {
				r_ptr = RP_NULL;
			}
		}
	}

	/*
	 * If the above produced nothing (as in a NULL r_ptr) we'll
	 * need to search up looking for a spot where the inode is
	 * on the left and go right from there.
	 */
	if (r_ptr == RP_NULL) {
		while ((ri = rv_inode(ri->rtti_parent)) != NULL) {
			rtt_ibit_t dir;

			bit = ri->rtti_bit;
			if (prune && bit >= st_bit) {
				/*
				 * Still in the subtree.  Continue up.
				 */
				continue;
			}
			if (!prune && bit < st_bit) {
				/*
				 * Fell off the top of the subtree.  Return
				 * a failure.
				 */
				return (NULL);
			}
			RN_KEYBIT(dir, bit, kp, k, which_k);
			if (dir == RTT_LEFT) {
				r_ptr = ri->rtti_right;
				if (r_ptr != RP_NULL) {
					break;
				}
			}
		}
		if (ri == NULL) {
			/*
			 * Fell off top of the tree.  There are no
			 * larger nodes than the one he gave us.
			 */
			return (NULL);
		}
	}

	/*
	 * We now have a non-NULL child pointer.  Go left through
	 * inodes until we find something attached or a child external.
	 */
	while (rp_is_int(r_ptr)) {
		ri = rp_to_int(rv_ptr(r_ptr));
#ifdef	RTTREE_VOLATILE
		/*
		 * XXX This code exists to produce a slightly
		 * better result if more than one modification
		 * is concurrently made while the walk is in
		 * progress.  The caller shouldn't allow this...
		 *
		 * In a volatile tree keep going around until we
		 * find something.  Read them in reverse order from
		 * priority so that the pointer we want most is
		 * read last.  An inode, other than the root, will
		 * never have all three pointers NULL, but we might
		 * transiently see them that way so if we do just
		 * try again.
		 */
		bit = ri->rtti_bit;
		do {
			rtt_ptr_t l_ptr, a_ptr;

			r_ptr = ri->rtti_right;
			l_ptr = ri->rtti_left;
			a_ptr = ri->rtti_attached;
			if (a_ptr != RP_NULL) {
				r_ptr = a_ptr;
			} else if (l_ptr != RP_NULL) {
				r_ptr = l_ptr;
			}
		} while (__predict_false(r_ptr == RP_NULL) && bit != 0);
#else	/* RTTREE_VOLATILE */
		r_ptr = ri->rtti_attached;
		if (r_ptr == RP_NULL) {
			r_ptr = ri->rtti_left;
			if (r_ptr == RP_NULL) {
				r_ptr = ri->rtti_right;
			}
		}
#endif	/* RTTREE_VOLATILE */
	}

	/*
	 * Return the child.
	 */
	return (rp_to_ext(rv_ptr(r_ptr)));
}


/*
 * rn_prev()
 *
 * Like rn_next(), but moving through the tree in the opposite direction.
 * I'd like to combine both into a single rn_forw(), but the asymmetry
 * of dealing with attached nodes makes the functions distinctly different.
 *
 * The node given to the function no longer needs to be the first
 * rtnode_t with the key.
 */
static inline rtnode_t *
rn_prev(rttree_t *, rtnode_t *, bool, rtt_ibit_t) ALWAYS_INLINE;

static inline rtnode_t *
rn_prev (rttree_t *rr, rtnode_t *rn, bool prune, rtt_ibit_t st_bit)
{
	V_ rtt_inode_t *ri;
	V_ rtnode_t *v_rn;
	rtt_ptr_t r_ptr;
	rtt_ibit_t bit, which_k;
	const uint32_t *kp;
	uint32_t k;

	kp = NULL;
	RN_KEYBIT_INIT(RN_NOBIT, kp, k, which_k);
	if (rn == NULL) {
		/*
		 * A subtree operation with a prefix length greater
		 * than zero makes sense without an external to provide
		 * the prefix.
		 */
		if (!prune && st_bit > 0) {
			return (NULL);
		}
		r_ptr = rp_from_int(&rr->rttr_root);
		ri = NULL;
	} else {
		kp = rn_key(rr, rn);
		v_rn = rn;
		ri = rv_inode(v_rn->rttn_parent);
		bit = ri->rtti_bit;
		if (!prune && bit < st_bit) {
			/*
			 * Either prefix too short to be in subtree
			 * he says it is, or we've already fallen
			 * off the top of the subtree.
			 */
			return (NULL);
		}
		if (rn->rttn_len == bit || (prune && bit >= st_bit)) {
			/*
			 * Either he's attached or we're in the subtree
			 * we're pruning.  In either case we must head back
			 * up.
			 */
			r_ptr = RP_NULL;
		} else {
			rtt_ibit_t dir;

			/*
			 * See which side he's on from his key.  If he's
			 * right go left, otherwise take the attached.
			 */
			RN_KEYBIT(dir, bit, kp, k, which_k);
			if (dir != RTT_LEFT) {
				r_ptr = ri->rtti_left;
				if (r_ptr == RP_NULL) {
					r_ptr = ri->rtti_attached;
				}
			} else {
				r_ptr = ri->rtti_attached;
			}
		}
	}

	/*
	 * If we got nothing from above, head back up the
	 * tree looking for places we went right and go
	 * left from there.  If we're on the left look instead
	 * at the attached pointer.
	 */
	if (r_ptr == RP_NULL) {
		while ((ri = rv_inode(ri->rtti_parent)) != NULL) {
			rtt_ibit_t dir;

			bit = ri->rtti_bit;
			if (prune && bit >= st_bit) {
				/*
				 * Pruning and still in the subtree.  Keep
				 * going up.
				 */
				continue;
			}
			if (!prune && bit < st_bit) {
				/*
				 * Subtree operation, and we've fallen
				 * off the top of the tree.  Return failure.
				 */
				return (NULL);
			}
			RN_KEYBIT(dir, bit, kp, k, which_k);
			if (dir != RTT_LEFT) {
				/*
				 * On the right, check left.
				 */
				r_ptr = ri->rtti_left;
				if (r_ptr != RP_NULL) {
					break;
				}
			}
			r_ptr = ri->rtti_attached;
			if (r_ptr != RP_NULL) {
				break;
			}
		}
		if (ri == NULL) {
			/*
			 * Fell off the tree top.
			 */
			return (NULL);
		}
	}

	/*
	 * We've got a pointer to somewhere we haven't been before.  If
	 * it is an inode, move exclusively right looking for a child
	 * external.
	 */
	while (rp_is_int(r_ptr)) {
		ri = rp_to_int(rv_ptr(r_ptr));
#ifdef RTTREE_VOLATILE
		/*
		 * XXX This code exists to produce a slightly
		 * better result if more than one modification
		 * is concurrently made while the walk is in
		 * progress.  The caller shouldn't allow this...
		 *
		 * In a volatile tree keep going around until we find
		 * something.  Read them in reverse order from priority
		 * so that the pointer we want most is read last.  An
		 * inode, other than the root inode, will never have
		 * all pointers NULL so if we see them that way it is
		 * a transient state which will fix itself.
		 */
		bit = ri->rtti_bit;
		do {
			rtt_ptr_t l_ptr, a_ptr;

			a_ptr = ri->rtti_attached;
			l_ptr = ri->rtti_left;
			r_ptr = ri->rtti_right;
			if (r_ptr == RP_NULL) {
				if (l_ptr != RP_NULL) {
					r_ptr = l_ptr;
				} else {
					r_ptr = a_ptr;
				}
			}
		} while (__predict_false(r_ptr == RP_NULL) && bit != 0);
#else	/* RTTREE_VOLATILE */
		r_ptr = ri->rtti_right;
		if (r_ptr == RP_NULL) {
			r_ptr = ri->rtti_left;
			if (r_ptr == RP_NULL) {
				r_ptr = ri->rtti_attached;
			}
		}
#endif	/* RTTREE_VOLATILE */
	}

	/*
	 * Return what we found.
	 */
	return (rp_to_ext(rv_ptr(r_ptr)));
}


/*
 * rn_next_cx()
 *
 * Perform a colored and/or match function _next() operation.  This
 * is called with constant boolean arguments indicating the variant type
 * of operation being performed; hopefully the compiler will eat the
 * unused code and provide well-optimized functions for the specific
 * task.
 */
static inline rtnode_t *
rn_next_cx (rttree_t *rr, rtnode_t *a_rn, bool prune, rtt_ibit_t st_bit,
	    bool sub_search,
	    bool has_color, rtt_colormask_t c_ignore, rtt_colormask_t c_skip,
	    bool has_gorp, const void *cmp_gorp)
{
	rtnode_t *rn, *first_rn;
	V_ rtnode_t *v_rn;

	/*
	 * If called with no argument we'll need to call rn_next()
	 * before doing anything.  If he's the last guy in his list
	 * we'll also need to do that.
	 */
	first_rn = a_rn;
	if (sub_search) {
		/*
		 * Called as part of a rttree_sub_first_*() or
		 * rttree_sub_last_*() execution.  Just start
		 * the search at the argument.
		 */
		rn = a_rn;
	} else if (prune || a_rn == NULL) {
		/*
		 * We'll be calling rn_next() right away for this,
		 * so there's no pressing need to check for unreasonable
		 * arguments here.  rn_next() can do it.
		 */
		rn = NULL;
	} else {
		/*
		 * If doing a subtree walk make sure the argument
		 * node is in the subtree it claims to be.
		 */
		if (a_rn->rttn_len < st_bit) {
			return (NULL);
		}
		v_rn = a_rn;
		rn = rp_to_ext(rv_ptr(v_rn->rttn_next));
	}

	/*
	 * Loop until we find something.
	 */
	do {
		while (rn) {
			int res;

			res = rn_match(rr, rn, has_color, c_ignore, c_skip,
				       has_gorp, cmp_gorp);
			if (res >= RTT_CMP_EQ) {
				if (res == RTT_CMP_EQ) {
					return (rn);
				}
				break;		/* it was RTT_CMP_GT */
			}
			v_rn = rn;
			rn = rp_to_ext(rv_ptr(v_rn->rttn_next));
		}

		/*
		 * Didn't find anything in that list.  Look for the
		 * next one.
		 */
		rn = first_rn = rn_next(rr, first_rn, prune, st_bit);
	} while (rn);

	/*
	 * If we got here we didn't find anything to match.
	 */
	return (NULL);
}


/*
 * rn_prev_cx()
 *
 * Like the above but works in the opposite direction.
 */
static inline rtnode_t *
rn_prev_cx (rttree_t *rr, rtnode_t *a_rn, bool prune, rtt_ibit_t st_bit,
	    bool sub_search,
	    bool has_color, rtt_colormask_t c_ignore, rtt_colormask_t c_skip,
	    bool has_gorp, const void *cmp_gorp)
{
	rtnode_t *first_rn, *last_rn;
	V_ rtnode_t *v_rn;

	/*
	 * If called with no argument we'll need to call rn_next()
	 * before doing anything.  If he's the last guy in his list
	 * we'll also need to do that.
	 */
	if (sub_search) {
		first_rn = a_rn;
		last_rn = NULL;
	} else if (a_rn == NULL) {
		/*
		 * We'll be calling rn_next() right away for this,
		 * so there's no pressing need to check for unreasonable
		 * arguments here.  rn_next() can do it.
		 */
		first_rn = last_rn = NULL;
	} else {
		/*
		 * If doing a subtree walk make sure the argument
		 * node is in the subtree it claims to be.  In the
		 * prune case we'll arrange to call rn_prev()
		 * immediately.
		 */
		if (!prune && a_rn->rttn_len < st_bit) {
			return (NULL);
		}
		last_rn = a_rn;
		if (prune) {
			first_rn = a_rn;
		} else {
			first_rn = rn_first(rr, a_rn);
			if (first_rn == NULL) {
				first_rn = a_rn;
			}
		}
	}

	/*
	 * Loop until we find something.  We check all the way
	 * to the end of each destination list, or until we get an ignore,
	 * since it is the last match we are interested in.  This
	 * sucks, but the data structure design makes _next_cx() easy
	 * and _prev_cx() hard so we live with it.
	 */
	do {
		rtnode_t *rn;
		rtnode_t *match_rn = NULL;

		rn = first_rn;
		while (rn != last_rn) {
			int res;

			res = rn_match(rr, rn, has_color, c_ignore, c_skip,
				       has_gorp, cmp_gorp);
			if (res >= RTT_CMP_EQ) {
				if (res != RTT_CMP_EQ) {
					break;		/* it was RTT_CMP_GT */
				}
				match_rn = rn;
			}
			v_rn = rn;
			rn = rp_to_ext(rv_ptr(v_rn->rttn_next));
		}

		/*
		 * match_rn will point at the last match we found.
		 */
		if (match_rn != NULL) {
			return (match_rn);
		}

		/*
		 * Didn't find what we where looking for.  Get
		 * the previous destination.
		 */
		first_rn = rn_prev(rr, first_rn, prune, st_bit);
		last_rn = NULL;
	} while (first_rn);

	/*
	 * If we got here we didn't find anything to match.
	 */
	return (NULL);
}

#endif	/* RTTREE_NEXT_H_ */
