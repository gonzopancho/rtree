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
 * rttree_getnext.h
 *
 * Inline functions to do _getnext() and _getprev() operations of
 * various sorts.  Not all of these may appear at the user interface.
 */
#ifndef	RTTREE_GETNEXT_H_
#define	RTTREE_GETNEXT_H_
#include <rttree_defs.h>

/*
 * rn_getforw()
 *
 * This inline provides basic full-tree, subtree and pruned _getnext()
 * and _getprev() operations.  It always returns the first node with a
 * particular destination.  The rules for volatile lookups apply: read every
 * field just once and try to access data which may be concurrently
 * changing through pointers declared volatile to encourage the compiler
 * to actually do it this way.  Note that the forw_dir argument should
 * be RTT_RIGHT for a _getnext() and RTT_LEFT for a _getprev(); the code
 * which calls the inline will probably be setting the argument to a
 * constant value.  Note that the asymmetry of _getnext() and _getprev()
 * operations make implementing both directions in one function not
 * so pretty, but they still share so much code that I'd hate to
 * cut-and-paste this into two functions to keep debugged.
 */
static inline rtnode_t *
rn_getforw (rttree_t *rr, const uint32_t *kp, rtt_ibit_t plen,
	    bool ret_match, rtt_ibit_t forw_dir, bool prune, rtt_ibit_t st_bit)
{
	V_ rtt_inode_t *ri;
	rtnode_t *rn, *candidate_rn;
	rtt_ibit_t d_bit, bit, his_len, which_k;
	uint32_t k;
	rtt_ptr_t r_ptr, check_ptr;
	rtt_lut_t lut;
	bool down_search_only;
	const rtt_ibit_t forw = (forw_dir & 0x1);
	const rtt_ibit_t back = RN_NOT(forw_dir & 0x1);

	/*
	 * Catch some stupid cases.  We can't do a subtree search
	 * if the prefix is too short to be in the subtree.  We
	 * can't do a prune with a zero-length subtree since the
	 * latter includes the whole tree.
	 */
	if (!prune && plen < st_bit) {
		return (NULL);
	}
	if (prune && st_bit == 0) {
		return (NULL);
	}

	/*
	 * We'll use the LUTs to do the initial search, but stop
	 * when we get to one which tests anything beyond the end
	 * of the prefix and do a bit-by-bit search from there
	 * instead.
	 */
	r_ptr = rp_from_int(rv_inode(&rr->rttr_root));
	ri = rp_to_int(r_ptr);
	lut = lut_make(NULL, RTT_LUT_2);
	RN_KEYLUT_INIT(RN_NOBIT, kp, k, which_k);
	while ((bit = ri->rtti_bit) < plen) {
		uint32_t l_word;

		lut = ri->rtti_lut;
		if (plen <= (bit + lut_isize(lut))) {
			r_ptr = RP_NULL;
			break;
		}
		RN_KEYLUT(l_word, bit, kp, k, which_k);
		lut = rv_lut(lut);
		r_ptr = lut_ptr(lut)[l_word >> lut_ishift(lut)];
		if (rp_is_ext(r_ptr)) {
			break;
		}
		ri = rp_to_int(rv_ptr(r_ptr));
	}

	RN_KEYLUT_TO_KEYBIT(which_k);
	if (bit < plen) {
		rtt_ibit_t max_bit;

		/*
		 * Maybe didn't makt it to our prefix length.  If
		 * we got a child external from the LUTs it is our
		 * best match.  Make sure `ri' points at his parent
		 * inode, though, since its value now is the place
		 * where the LUT is.  And before we do any of that
		 * capture the bit that the LUT on the inode we
		 * have now extends to so we know how far to search.
		 */
		if (r_ptr != RP_NULL) {
			ri = rp_to_ext(rv_ptr(r_ptr))->rttn_parent;
			if (ri->rtti_attached != r_ptr ||
			    ri->rtti_bit >= plen) {
				goto got_one;
			}
		}

		/*
		 * At this point we either got a NULL or attached
		 * guy out of the last LUT lookup.  That means
		 * that there is a difference between our key and the
		 * tree contents in the bits covered by the LUT.
		 * Search bit-by-bit against our key to that
		 * depth to make sure we capture that.
		 */
		max_bit = bit + lut_isize(lut) + 1;
		if (max_bit > plen) {
			max_bit = plen;
		}
		while ((bit = ri->rtti_bit) < max_bit) {
			rtt_ibit_t dir;

			RN_KEYBIT(dir, bit, kp, k, which_k);
#ifdef RTTREE_VOLATILE
			/*
			 * XXX This code is here as an attempt to get
			 * a better result if the caller allows more
			 * than one concurrent modification to the tree
			 * while this is active.  The caller shouldn't do
			 * that...
			 *
			 * Because we can't atomically sample all three
			 * pointers in an inode it is (remotely) possible
			 * to see all three with NULL values in a non-root
			 * inode in a volatile tree; they won't have all
			 * been NULL at once, but they all could have been
			 * when they were sampled.  If this happens anywhere
			 * other than the root inode (it could legitimately
			 * happen here in the tree is empty) we try again;
			 * the probability of this happening twice 
			 * is miniscule.
			 */
			do {
				rtt_ptr_t n_ptr = ri->rtti_child[RN_NOT(dir)];
				rtt_ptr_t a_ptr = ri->rtti_attached;

				r_ptr = ri->rtti_child[dir];
				if (r_ptr == RP_NULL) {
					if (a_ptr != RP_NULL) {
						r_ptr = a_ptr;
					} else {
						r_ptr = n_ptr;
					}
				}
			} while (__predict_false(r_ptr == RP_NULL) && bit != 0);
			if (rp_is_ext(r_ptr)) {
				goto got_one;
			}
#else	/* RTTREE_VOLATILE */
			/*
			 * In a non-volatile tree we don't have the problem.
			 * In a non-root inode at least one of the pointers
			 * will be non-NULL.  We break out here when we take
			 * a pointer other than the `dir' one because it
			 * is simple to do here, and we know it will
			 * capture the difference between the key and
			 * the tree.
			 */
			r_ptr = ri->rtti_child[dir];
			if (rp_is_ext(r_ptr)) {
				if (r_ptr == RP_NULL) {
					r_ptr = ri->rtti_attached;
					if (r_ptr == RP_NULL) {
						r_ptr =
						    ri->rtti_child[RN_NOT(dir)];
					}
				}
				break;
			}
#endif	/* RTTREE_VOLATILE */
			ri = rp_to_int(rv_ptr(r_ptr));
		}
	}

	/*
	 * If r_ptr is not an external we'll need to find one; either
	 * we're past the end of his prefix or we know the part of
	 * the tree we're in already differs from the key.  Go back
	 * when we can and take an attached node if we find one.
	 */
	while (rp_is_int(r_ptr)) {
		ri = rp_to_int(rv_ptr(r_ptr));
		bit = ri->rtti_bit;

#ifdef RTTREE_VOLATILE
		/*
		 * Volatile tree.  Like the above, including the XXX.
		 */
		do {
			rtt_ptr_t b_ptr, f_ptr;

			b_ptr = ri->rtti_child[back];
			f_ptr = ri->rtti_child[forw];
			r_ptr = ri->rtti_attached;
			if (r_ptr == RP_NULL) {
				if (b_ptr != RP_NULL) {
					r_ptr = b_ptr;
				} else {
					r_ptr = f_ptr;
				}
			}
		} while (__predict_false(r_ptr == RP_NULL) && bit != 0);
#else	/* RTTREE_VOLATILE */
		r_ptr = ri->rtti_attached;
		if (r_ptr != RP_NULL) {
			break;
		}
		r_ptr = ri->rtti_child[back];
		if (r_ptr == RP_NULL) {
			r_ptr = ri->rtti_child[forw];
		}
#endif	/* RTTREE_VOLATILE */
	}

	/*
	 * When we get here we've found an external pointer (which
	 * might be RP_NULL), with ri being the inode the external
	 * was found in.  Do a comparison with our key to the
	 * length of the shorter.
	 */
got_one:
	rn = rp_to_ext(rv_ptr(r_ptr));
	if (rn == NULL) {
		/*
		 * This should only happen in an uninitialized tree,
		 * if I haven't misunderstood the possible transient
		 * states.  Return NULL.
		 */
		return (NULL);
	}
	his_len = rn->rttn_len;
	d_bit = rn_cmp(kp, rn_key(rr, rn), ((plen < his_len) ? plen : his_len));


	/*
	 * Determine what we've got based on d_bit.
	 */
	candidate_rn = NULL;
	check_ptr = RP_NULL;
	down_search_only = false;
	if (!prune && d_bit < st_bit) {
		/*
		 * Subtree search, but the node we found isn't
		 * in the subtree.  There's no hope.
		 */
		return (NULL);
	}
	if (prune && d_bit >= st_bit) {
		/*
		 * Prune search, and we got a node in the
		 * subtree.  Go up and forw above a node testing
		 * st_bit.
		 */
		d_bit = st_bit;
	} else if (d_bit >= plen) {
		/*
		 * If his prefix is the same length, so we've got a match
		 * for the argument.  If ret_match is true, return it.
		 */
		if (ret_match && his_len == plen) {
			return (rn);
		}

		/*
		 * If the search is RTT_LEFT we have no choice but to go
		 * up and forward.  For an RTT_RIGHT search there are
		 * a couple of options.
		 */
		if (forw == RTT_RIGHT) {
			/*
			 * The thing we found matched the argument key to
			 * the full length of the prefix.  If we're doing
			 * an RTT_RIGHT search and his prefix length is
			 * greater than our's he's next due to the leftward
			 * lean of the above search beyond theprefix.
			 */
			if (his_len > plen) {
				return (rn);
			}

			/*
			 * For an RTT_RIGHT search, if we're attached there
			 * may be a longer guy in the tree below us which
			 * would be 'next'.  This isn't guaranteed, though,
			 * since this might be the root inode or a
			 * half-deleted guy.  If we find one we leave it
			 * r_ptr and set down_search_only to look for it at
			 * the bottom.
			 */
			if (bit == plen) {
				rtt_ptr_t rt_ptr = ri->rtti_right;

				r_ptr = ri->rtti_left;
				if (r_ptr == RP_NULL) {
					r_ptr = rt_ptr;
				}
				if (r_ptr != RP_NULL) {
					down_search_only = true;
				}
			}
		}
	} else if (d_bit >= his_len) {
		/*
		 * Here we fully match the prefix we found to his length,
		 * but he's shorter than our length; he's previous to our
		 * guy.  If forw==RTT_LEFT he's an acceptable return value,
		 * but we don't know that he's the back-most (right-most)
		 * such value; if he's attached stuff below him in the
		 * tree may still be forward of us but will be back of him.
		 * For a forw==RTT_RIGHT search, if he's attached, we may
		 * find something we can return below him in the forward
		 * direction (they'll be be on the right side of us), but
		 * otherwise we will need to go up and forward.
		 */
		if (his_len == ri->rtti_bit &&
		    (r_ptr = ri->rtti_child[forw]) != RP_NULL) {
			/*
			 * Search down from here.
			 */
			down_search_only = true;
		} else if (forw == RTT_LEFT) {
			return (rn);
		}
	} else {
		rtt_ibit_t dir;

		/*
		 * Here d_bit is less than the length of both our prefix
		 * and the guy we found.  We need to know the state of
		 * the bit that is different in our key to tell what to
		 * do.
		 */
		RN_KEYBIT(dir, d_bit, kp, k, which_k);
		if (dir == back) {
			/*
			 * We are back of him, so he is forward of us.
			 * He is something we could return.  The problem
			 * is that there may be nodes which are back of
			 * him but still forward of us, so we'll need to
			 * check for these.  Note him as a candidate.  Note
			 * that if the search is RTT_LEFT and he's attached
			 * a better guy might be below him in the tree,
			 * so note that as well.
			 */
			candidate_rn = rn;
			if (forw == RTT_LEFT && bit == his_len) {
				check_ptr = ri->rtti_child[back];
				if (check_ptr == RP_NULL) {
					check_ptr = ri->rtti_child[forw];
				}
			}
		} else {
			/*
			 * Our key is forward of his.  We'll need to go
			 * up and forward to get past the argument key.
			 */
		}
	}

	/*
	 * The state of candidate_rn tells us how to proceed.
	 */
	if (candidate_rn) {
		rtt_ptr_t c_ptr = check_ptr;

		/*
		 * We have a candidate, but need to find a more backward
		 * one if there is any.  Head up the tree looking for a
		 * attached guys or a place we could have gone left
		 * from inodes testing bits > d_bit.
		 */
		bit = ri->rtti_bit;
		if (bit > d_bit) {
			kp = rn_key(rr, candidate_rn);
			RN_KEYBIT_INIT(bit, kp, k, which_k);
			if (his_len == bit) {
				ri = rv_inode(ri->rtti_parent);
			}
			while (ri != NULL && (bit = ri->rtti_bit) > d_bit) {
				/*
				 * For a forw==RTT_LEFT search we should
				 * not look for attached nodes.
				 */
				if (forw == RTT_RIGHT) {
					r_ptr = ri->rtti_attached;
				} else {
					r_ptr = RP_NULL;
				}
				if (r_ptr != RP_NULL) {
					candidate_rn = rp_to_ext(rv_ptr(r_ptr));
					c_ptr = RP_NULL;
				} else {
					rtt_ibit_t dir;

					RN_KEYBIT(dir, bit, kp, k, which_k);
					if (dir == forw) {
						r_ptr = ri->rtti_child[back];
						if (r_ptr != RP_NULL) {
							c_ptr = r_ptr;
						}
					}
				}
				ri = rv_inode(ri->rtti_parent);
			}
		}

		if (c_ptr == RP_NULL) {
			return (candidate_rn);
		}
		r_ptr = c_ptr;
	} else if (!down_search_only) {
		/*
		 * Here we need to move up to nodes testing bits < d_bit,
		 * find one where we went back and go forward from there
		 * instead.  If d_bit is zero there's no hope (if there was
		 * something forward of the root inode we'd be there already),
		 * so catch that case.
		 */
		if (d_bit == 0) {
			return (NULL);
		}

		/*
		 * No need to check for a NULL parent pointer, d_bit is
		 * greater than zero so we'll stop somewhere.
		 */
		while ( (bit = ri->rtti_bit) >= d_bit) {
			ri = rv_inode(ri->rtti_parent);
		}

		r_ptr = RP_NULL;
		for (;;) {
			rtt_ibit_t dir;

			if (!prune && bit < st_bit) {
				return (NULL);
			}
			RN_KEYBIT(dir, bit, kp, k, which_k);
			if (dir == back) {
				r_ptr = ri->rtti_child[forw];
				if (r_ptr != RP_NULL) {
					break;
				}
			}
			if (forw == RTT_LEFT) {
				r_ptr = ri->rtti_attached;
				if (r_ptr != RP_NULL) {
					break;
				}
			}
			ri = rv_inode(ri->rtti_parent);
			if (!ri) {
				break;
			}
			bit = ri->rtti_bit;
		}

		if (r_ptr == RP_NULL) {
			/* No hope */
			return (NULL);
		}
	}

	/*
	 * Okay, at this point r_ptr refers to a part of the
	 * tree where the next guy lives.  Find the back-most guy
	 * under here. For forw==RTT_RIGHT we want the first attached
	 * we come across, otherwise we don't
	 */
	while (rp_is_int(r_ptr)) {
		ri = rp_to_int(rv_ptr(r_ptr));
#ifdef RTTREE_VOLATILE
		/*
		 * XXX This code exists only to try to get a
		 * better result when more than one tree modification
		 * is performed while we're doing this.  The
		 * caller shouldn't let that happen...
		 */
		do {
			rtt_ptr_t b1_ptr, b2_ptr;

			if (forw == RTT_RIGHT) {
				b2_ptr = ri->rtti_child[forw];
				b1_ptr = ri->rtti_child[back];
				r_ptr = ri->rtti_attached;
			} else {
				b2_ptr = ri->rtti_attached;
				b1_ptr = ri->rtti_child[forw];
				r_ptr = ri->rtti_child[back];
			}
			if (r_ptr == RP_NULL) {
				if (b1_ptr != RP_NULL) {
					r_ptr = b1_ptr;
				} else {
					r_ptr = b2_ptr;
				}
			}
		} while (__predict_false(r_ptr == RP_NULL));
#else	/* RTTREE_VOLATILE */
		if (forw == RTT_LEFT) {
			r_ptr = RP_NULL;
		} else {
			r_ptr = ri->rtti_attached;
		}
		if (r_ptr == RP_NULL) {
			r_ptr = ri->rtti_child[back];
			if (r_ptr == RP_NULL) {
				r_ptr = ri->rtti_child[forw];
				if (r_ptr == RP_NULL && forw == RTT_LEFT) {
					r_ptr = ri->rtti_attached;
				}
			}
		}
#endif	/* RTTREE_VOLATILE */
	}

	/*
	 * All done.  Return what we found.
	 */
	return (rp_to_ext(rv_ptr(r_ptr)));
}

#endif	/* RTTREE_GETNEXT_H_ */
