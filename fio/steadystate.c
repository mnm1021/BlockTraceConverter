#include <stdlib.h>

#include "fio.h"
#include "steadystate.h"
#include "helper_thread.h"

bool steadystate_enabled = false;

static void steadystate_alloc(struct thread_data *td)
{
	int i;

	td->ss.bw_data = malloc(td->ss.dur * sizeof(unsigned long));
	td->ss.iops_data = malloc(td->ss.dur * sizeof(unsigned long));
	/* initialize so that it is obvious if the cache is not full in the output */
	for (i = 0; i < td->ss.dur; i++)
		td->ss.iops_data[i] = td->ss.bw_data[i] = 0;
}

void steadystate_setup(void)
{
	int i, prev_groupid;
	struct thread_data *td, *prev_td;

	if (!steadystate_enabled)
		return;

	/*
	 * if group reporting is enabled, identify the last td
	 * for each group and use it for storing steady state
	 * data
	 */
	prev_groupid = -1;
	prev_td = NULL;
	for_each_td(td, i) {
		if (td->ts.ss == NULL)
			continue;

		if (!td->o.group_reporting) {
			steadystate_alloc(td);
			continue;
		}

		if (prev_groupid != td->groupid) {
			if (prev_td != NULL) {
				prev_td->ss.state |= __FIO_SS_LAST;
				steadystate_alloc(prev_td);
			}
			prev_groupid = td->groupid;
		}
		prev_td = td;
	}

	if (prev_td != NULL && prev_td->o.group_reporting) {
		prev_td->ss.state |= __FIO_SS_LAST;
		steadystate_alloc(prev_td);
	}
}

static bool steadystate_slope(unsigned long iops, unsigned long bw,
			      struct thread_data *td)
{
	int i, j;
	double result;
	struct steadystate_data *ss = &td->ss;
	unsigned long new_val;

	ss->bw_data[ss->tail] = bw;
	ss->iops_data[ss->tail] = iops;

	if (ss->state & __FIO_SS_IOPS)
		new_val = iops;
	else
		new_val = bw;

	if (ss->tail < ss->head || (ss->tail - ss->head == ss->dur - 1)) {
		if (ss->sum_y == 0) {	/* first time through */
			for(i = 0; i < ss->dur; i++) {
				if (ss->state & __FIO_SS_IOPS)
					ss->sum_y += ss->iops_data[i];
				else
					ss->sum_y += ss->bw_data[i];
				j = ss->head + i;
				if (j >= ss->dur)
					j -= ss->dur;
				if (ss->state & __FIO_SS_IOPS)
					ss->sum_xy += ss->iops_data[j];
				else
					ss->sum_xy += ss->bw_data[j];
			}
		} else {		/* easy to update the sums */
			ss->sum_y -= ss->oldest_y;
			ss->sum_y += new_val;
			ss->sum_xy = ss->sum_xy - ss->sum_y + ss->dur * new_val;
		}

		if (ss->state & __FIO_SS_IOPS)
			ss->oldest_y = ss->iops_data[ss->head];
		else
			ss->oldest_y = ss->bw_data[ss->head];

		/*
		 * calculate slope as (sum_xy - sum_x * sum_y / n) / (sum_(x^2)
		 * - (sum_x)^2 / n) This code assumes that all x values are
		 * equally spaced when they are often off by a few milliseconds.
		 * This assumption greatly simplifies the calculations.
		 */
		ss->slope = (ss->sum_xy - (double) ss->sum_x * ss->sum_y / ss->dur) /
				(ss->sum_x_sq - (double) ss->sum_x * ss->sum_x / ss->dur);
		if (ss->pct)
			ss->criterion = 100.0 * ss->slope / (ss->sum_y / ss->dur);
		else
			ss->criterion = ss->slope;

		dprint(FD_STEADYSTATE, "sum_y: %llu, sum_xy: %llu, slope: %f, "
					"criterion: %f, limit: %f\n",
					ss->sum_y, ss->sum_xy, ss->slope,
					ss->criterion, ss->limit);

		result = ss->criterion * (ss->criterion < 0.0 ? -1.0 : 1.0);
		if (result < ss->limit)
			return true;
	}

	ss->tail = (ss->tail + 1) % ss->dur;
	if (ss->tail <= ss->head)
		ss->head = (ss->head + 1) % ss->dur;

	return false;
}

static bool steadystate_deviation(unsigned long iops, unsigned long bw,
				  struct thread_data *td)
{
	int i;
	double diff;
	double mean;

	struct steadystate_data *ss = &td->ss;

	ss->bw_data[ss->tail] = bw;
	ss->iops_data[ss->tail] = iops;

	if (ss->tail < ss->head || (ss->tail - ss->head == ss->dur - 1)) {
		if (ss->sum_y == 0) {	/* first time through */
			for(i = 0; i < ss->dur; i++)
				if (ss->state & __FIO_SS_IOPS)
					ss->sum_y += ss->iops_data[i];
				else
					ss->sum_y += ss->bw_data[i];
		} else {		/* easy to update the sum */
			ss->sum_y -= ss->oldest_y;
			if (ss->state & __FIO_SS_IOPS)
				ss->sum_y += ss->iops_data[ss->tail];
			else
				ss->sum_y += ss->bw_data[ss->tail];
		}

		if (ss->state & __FIO_SS_IOPS)
			ss->oldest_y = ss->iops_data[ss->head];
		else
			ss->oldest_y = ss->bw_data[ss->head];

		mean = (double) ss->sum_y / ss->dur;
		ss->deviation = 0.0;

		for (i = 0; i < ss->dur; i++) {
			if (ss->state & __FIO_SS_IOPS)
				diff = ss->iops_data[i] - mean;
			else
				diff = ss->bw_data[i] - mean;
			ss->deviation = max(ss->deviation, diff * (diff < 0.0 ? -1.0 : 1.0));
		}

		if (ss->pct)
			ss->criterion = 100.0 * ss->deviation / mean;
		else
			ss->criterion = ss->deviation;

		dprint(FD_STEADYSTATE, "sum_y: %llu, mean: %f, max diff: %f, "
					"objective: %f, limit: %f\n",
					ss->sum_y, mean, ss->deviation,
					ss->criterion, ss->limit);

		if (ss->criterion < ss->limit)
			return true;
	}

	ss->tail = (ss->tail + 1) % ss->dur;
	if (ss->tail <= ss->head)
		ss->head = (ss->head + 1) % ss->dur;

	return false;
}

void steadystate_check(void)
{
	int i, j, ddir, prev_groupid, group_ramp_time_over = 0;
	unsigned long rate_time;
	struct thread_data *td, *td2;
	struct timeval now;
	unsigned long group_bw = 0, group_iops = 0;
	unsigned long long td_iops;
	unsigned long long td_bytes;
	bool ret;

	prev_groupid = -1;
	for_each_td(td, i) {
		struct steadystate_data *ss = &td->ss;

		if (!ss->dur || td->runstate <= TD_SETTING_UP ||
		    td->runstate >= TD_EXITED || (ss->state & __FIO_SS_ATTAINED))
			continue;

		td_iops = 0;
		td_bytes = 0;
		if (!td->o.group_reporting ||
		    (td->o.group_reporting && td->groupid != prev_groupid)) {
			group_bw = 0;
			group_iops = 0;
			group_ramp_time_over = 0;
		}
		prev_groupid = td->groupid;

		fio_gettime(&now, NULL);
		if (ss->ramp_time && !(ss->state & __FIO_SS_RAMP_OVER)) {
			/*
			 * Begin recording data one second after ss->ramp_time
			 * has elapsed
			 */
			if (utime_since(&td->epoch, &now) >= (ss->ramp_time + 1000000L))
				ss->state |= __FIO_SS_RAMP_OVER;
		}

		td_io_u_lock(td);
		for (ddir = DDIR_READ; ddir < DDIR_RWDIR_CNT; ddir++) {
			td_iops += td->io_blocks[ddir];
			td_bytes += td->io_bytes[ddir];
		}
		td_io_u_unlock(td);

		rate_time = mtime_since(&ss->prev_time, &now);
		memcpy(&ss->prev_time, &now, sizeof(now));

		/*
		 * Begin monitoring when job starts but don't actually use
		 * data in checking stopping criterion until ss->ramp_time is
		 * over. This ensures that we will have a sane value in
		 * prev_iops/bw the first time through after ss->ramp_time
		 * is done.
		 */
		if (ss->state & __FIO_SS_RAMP_OVER) {
			group_bw += 1000 * (td_bytes - ss->prev_bytes) / rate_time;
			group_iops += 1000 * (td_iops - ss->prev_iops) / rate_time;
			++group_ramp_time_over;
		}
		ss->prev_iops = td_iops;
		ss->prev_bytes = td_bytes;

		if (td->o.group_reporting && !(ss->state & __FIO_SS_LAST))
			continue;

		/*
		 * Don't begin checking criterion until ss->ramp_time is over
		 * for at least one thread in group
		 */
		if (!group_ramp_time_over)
			continue;

		dprint(FD_STEADYSTATE, "steadystate_check() thread: %d, "
					"groupid: %u, rate_msec: %ld, "
					"iops: %lu, bw: %lu, head: %d, tail: %d\n",
					i, td->groupid, rate_time, group_iops,
					group_bw, ss->head, ss->tail);

		if (td->o.ss & __FIO_SS_SLOPE)
			ret = steadystate_slope(group_iops, group_bw, td);
		else
			ret = steadystate_deviation(group_iops, group_bw, td);

		if (ret) {
			if (td->o.group_reporting) {
				for_each_td(td2, j) {
					if (td2->groupid == td->groupid) {
						td2->ss.state |= __FIO_SS_ATTAINED;
						fio_mark_td_terminate(td2);
					}
				}
			} else {
				ss->state |= __FIO_SS_ATTAINED;
				fio_mark_td_terminate(td);
			}
		}
	}
}

void td_steadystate_init(struct thread_data *td)
{
	struct steadystate_data *ss = &td->ss;
	struct thread_options *o = &td->o;

	memset(ss, 0, sizeof(*ss));

	if (!o->ss_dur)
		return;

	steadystate_enabled = true;
	o->ss_dur /= 1000000L;

	/* put all steady state info in one place */
	ss->dur = o->ss_dur;
	ss->limit = o->ss_limit.u.f;
	ss->ramp_time = o->ss_ramp_time;
	ss->pct = o->ss_pct;

	ss->state = o->ss;
	if (!td->ss.ramp_time)
		ss->state |= __FIO_SS_RAMP_OVER;

	ss->sum_x = o->ss_dur * (o->ss_dur - 1) / 2;
	ss->sum_x_sq = (o->ss_dur - 1) * (o->ss_dur) * (2*o->ss_dur - 1) / 6;

	td->ts.ss = ss;
}
