/*
 * libhdfs engine
 *
 * this engine helps perform read/write operations on hdfs cluster using
 * libhdfs. hdfs doesnot support modification of data once file is created.
 *
 * so to mimic that create many files of small size (e.g 256k), and this
 * engine select a file based on the offset generated by fio.
 *
 * thus, random reads and writes can also be achieved with this logic.
 *
 */

#include <math.h>
#include <hdfs.h>

#include "../fio.h"
#include "../optgroup.h"

#define CHUNCK_NAME_LENGTH_MAX 80
#define CHUNCK_CREATION_BUFFER_SIZE 65536

struct hdfsio_data {
	hdfsFS fs;
	hdfsFile fp;
	uint64_t curr_file_id;
};

struct hdfsio_options {
	void *pad;			/* needed because offset can't be 0 for a option defined used offsetof */
	char *host;
	char *directory;
	unsigned int port;
	unsigned int chunck_size;
	unsigned int single_instance;
	unsigned int use_direct;
};

static struct fio_option options[] = {
	{
		.name	= "namenode",
		.lname	= "hfds namenode",
		.type	= FIO_OPT_STR_STORE,
		.off1   = offsetof(struct hdfsio_options, host),
		.def    = "localhost",
		.help	= "Namenode of the HDFS cluster",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= "hostname",
		.lname	= "hfds namenode",
		.type	= FIO_OPT_STR_STORE,
		.off1   = offsetof(struct hdfsio_options, host),
		.def    = "localhost",
		.help	= "Namenode of the HDFS cluster",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= "port",
		.lname	= "hdfs namenode port",
		.type	= FIO_OPT_INT,
		.off1	= offsetof(struct hdfsio_options, port),
		.def    = "9000",
		.minval	= 1,
		.maxval	= 65535,
		.help	= "Port used by the HDFS cluster namenode",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= "hdfsdirectory",
		.lname	= "hfds directory",
		.type	= FIO_OPT_STR_STORE,
		.off1   = offsetof(struct hdfsio_options, directory),
		.def    = "/",
		.help	= "The HDFS directory where fio will create chuncks",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= "chunck_size",
		.type	= FIO_OPT_INT,
		.off1	= offsetof(struct hdfsio_options, chunck_size),
		.def    = "1048576",
		.help	= "Size of individual chunck",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= "single_instance",
		.type	= FIO_OPT_BOOL,
		.off1	= offsetof(struct hdfsio_options, single_instance),
		.def    = "1",
		.help	= "Use a single instance",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= "hdfs_use_direct",
		.type	= FIO_OPT_BOOL,
		.off1	= offsetof(struct hdfsio_options, use_direct),
		.def    = "0",
		.help	= "Use readDirect instead of hdfsRead",
		.category = FIO_OPT_C_ENGINE,
		.group	= FIO_OPT_G_HDFS,
	},
	{
		.name	= NULL,
	},
};


static int get_chunck_name(char *dest, char *file_name, uint64_t chunk_id) {
	return snprintf(dest, CHUNCK_NAME_LENGTH_MAX, "%s_%lu", file_name, chunk_id);
}

static int fio_hdfsio_prep(struct thread_data *td, struct io_u *io_u)
{
	struct hdfsio_options *options = td->eo;
	struct hdfsio_data *hd = td->io_ops_data;
	unsigned long f_id;
	char fname[CHUNCK_NAME_LENGTH_MAX];
	int open_flags;

	/* find out file id based on the offset generated by fio */
	f_id = floor(io_u->offset / options-> chunck_size);

	if (f_id == hd->curr_file_id) {
		/* file is already open */
		return 0;
	}

	if (hd->curr_file_id != -1) {
		if ( hdfsCloseFile(hd->fs, hd->fp) == -1) {
			log_err("hdfs: unable to close file: %s\n", strerror(errno));
			return errno;
		}
		hd->curr_file_id = -1;
	}

	if (io_u->ddir == DDIR_READ || io_u->ddir == DDIR_SYNC) {
		open_flags = O_RDONLY;
	} else if (io_u->ddir == DDIR_WRITE) {
		open_flags = O_WRONLY;
	} else {
		log_err("hdfs: Invalid I/O Operation\n");
		return 0;
	}
	
	get_chunck_name(fname, io_u->file->file_name, f_id);
	hd->fp = hdfsOpenFile(hd->fs, fname, open_flags, 0, 0,
			      options->chunck_size);
	if(hd->fp == NULL) {
		log_err("hdfs: unable to open file: %s: %d\n", fname, strerror(errno));
		return errno;
	}
	hd->curr_file_id = f_id;

	return 0;
}

static int fio_hdfsio_queue(struct thread_data *td, struct io_u *io_u)
{
	struct hdfsio_data *hd = td->io_ops_data;
	struct hdfsio_options *options = td->eo;
	int ret;
	unsigned long offset;
	
	offset = io_u->offset % options->chunck_size;
	
	if( (io_u->ddir == DDIR_READ || io_u->ddir == DDIR_WRITE) && 
	     hdfsTell(hd->fs, hd->fp) != offset && hdfsSeek(hd->fs, hd->fp, offset) != 0 ) {
		log_err("hdfs: seek failed: %s, are you doing random write smaller than chunck size ?\n", strerror(errno));
		io_u->error = errno;
		return FIO_Q_COMPLETED;
	};

	// do the IO
	if (io_u->ddir == DDIR_READ) {
		if (options->use_direct) {
			ret = readDirect(hd->fs, hd->fp, io_u->xfer_buf, io_u->xfer_buflen);
		} else {
			ret = hdfsRead(hd->fs, hd->fp, io_u->xfer_buf, io_u->xfer_buflen);
		}
	} else if (io_u->ddir == DDIR_WRITE) {
		ret = hdfsWrite(hd->fs, hd->fp, io_u->xfer_buf,
				io_u->xfer_buflen);
	} else if (io_u->ddir == DDIR_SYNC) {
		ret = hdfsFlush(hd->fs, hd->fp);
	} else {
		log_err("hdfs: Invalid I/O Operation: %d\n", io_u->ddir);
		ret = EINVAL;
	}

	// Check if the IO went fine, or is incomplete
	if (ret != (int)io_u->xfer_buflen) {
		if (ret >= 0) {
			io_u->resid = io_u->xfer_buflen - ret;
			io_u->error = 0;
			return FIO_Q_COMPLETED;
		} else {
			io_u->error = errno;
		}
	}

	if (io_u->error)
		td_verror(td, io_u->error, "xfer");

	return FIO_Q_COMPLETED;
}

int fio_hdfsio_open_file(struct thread_data *td, struct fio_file *f)
{
	if (td->o.odirect) {
		td->error = EINVAL;
		return 0;
	}

	return 0;
}

int fio_hdfsio_close_file(struct thread_data *td, struct fio_file *f)
{
	struct hdfsio_data *hd = td->io_ops_data;

	if (hd->curr_file_id != -1) {
		if ( hdfsCloseFile(hd->fs, hd->fp) == -1) {
			log_err("hdfs: unable to close file: %s\n", strerror(errno));
			return errno;
		}
		hd->curr_file_id = -1;
	}
	return 0;
}

static int fio_hdfsio_init(struct thread_data *td)
{
	struct hdfsio_options *options = td->eo;
	struct hdfsio_data *hd = td->io_ops_data;
	struct fio_file *f;
	uint64_t j,k;
	int i, failure = 0;
	uint8_t buffer[CHUNCK_CREATION_BUFFER_SIZE];
	uint64_t bytes_left;	
	char fname[CHUNCK_NAME_LENGTH_MAX];	
	hdfsFile fp;
	hdfsFileInfo *fi;
	tOffset fi_size;

	for_each_file(td, f, i) {
		k = 0;
		for(j=0; j < f->real_file_size; j += options->chunck_size) {
			get_chunck_name(fname, f->file_name, k++);
			fi = hdfsGetPathInfo(hd->fs, fname);
			fi_size = fi ? fi->mSize : 0;
			// fill exist and is big enough, nothing to do
			if( fi && fi_size >= options->chunck_size) {
				continue;
			}
			fp = hdfsOpenFile(hd->fs, fname, O_WRONLY, 0, 0,
					  options->chunck_size);
			if(fp == NULL) {
				failure = errno;
				log_err("hdfs: unable to prepare file chunk %s: %s\n", fname, strerror(errno));
				break;
			}
			bytes_left = options->chunck_size;
			memset(buffer, 0, CHUNCK_CREATION_BUFFER_SIZE);
			while( bytes_left > CHUNCK_CREATION_BUFFER_SIZE) {
				if( hdfsWrite(hd->fs, fp, buffer, CHUNCK_CREATION_BUFFER_SIZE)
				    != CHUNCK_CREATION_BUFFER_SIZE) {
    					failure = errno;
	    				log_err("hdfs: unable to prepare file chunk %s: %s\n", fname, strerror(errno));
					break;
				};
				bytes_left -= CHUNCK_CREATION_BUFFER_SIZE;
			}
			if(bytes_left > 0) {
				if( hdfsWrite(hd->fs, fp, buffer, bytes_left)
				    != bytes_left) {
					failure = errno;
					break;
				};
			}
			if( hdfsCloseFile(hd->fs, fp) != 0) {
				failure = errno;
				log_err("hdfs: unable to prepare file chunk %s: %s\n", fname, strerror(errno));
				break;
			}
		}
		if(failure) {
			break;
		}
	}
	
	if( !failure ) {
		fio_file_set_size_known(f);
	}

	return failure;
}

static int fio_hdfsio_setup(struct thread_data *td)
{
	struct hdfsio_data *hd;
	struct fio_file *f;
	int i;
	uint64_t file_size, total_file_size;

	if (!td->io_ops_data) {
		hd = malloc(sizeof(*hd));
		memset(hd, 0, sizeof(*hd));
		
		hd->curr_file_id = -1;

		td->io_ops_data = hd;
	}
	
	total_file_size = 0;
	file_size = 0;

	for_each_file(td, f, i) {
		if(!td->o.file_size_low) {
			file_size = floor(td->o.size / td->o.nr_files);
			total_file_size += file_size;
		}
		else if (td->o.file_size_low == td->o.file_size_high)
			file_size = td->o.file_size_low;
		else {
			file_size = get_rand_file_size(td);
		}
		f->real_file_size = file_size;
	}
	/* If the size doesn't divide nicely with the chunck size,
	 * make the last files bigger.
	 * Used only if filesize was not explicitely given
	 */
	if (!td->o.file_size_low && total_file_size < td->o.size) {
		f->real_file_size += (td->o.size - total_file_size);
	}

	return 0;
}

static int fio_hdfsio_io_u_init(struct thread_data *td, struct io_u *io_u)
{
	struct hdfsio_data *hd = td->io_ops_data;
	struct hdfsio_options *options = td->eo;
	int failure;
	struct hdfsBuilder *bld;

	if (options->host == NULL || options->port == 0) {
		log_err("hdfs: server not defined\n");
		return EINVAL;
	}
	
	bld = hdfsNewBuilder();
	if (!bld) {
		failure = errno;
		log_err("hdfs: unable to allocate connect builder\n");
		return failure;
	}
	hdfsBuilderSetNameNode(bld, options->host);
	hdfsBuilderSetNameNodePort(bld, options->port);
	if(! options->single_instance) {
		hdfsBuilderSetForceNewInstance(bld);
	}
	hd->fs = hdfsBuilderConnect(bld);
	
	/* hdfsSetWorkingDirectory succeed on non existend directory */
	if (hdfsExists(hd->fs, options->directory) < 0 || hdfsSetWorkingDirectory(hd->fs, options->directory) < 0) {
		failure = errno;
		log_err("hdfs: invalid working directory %s: %s\n", options->directory, strerror(errno));
		return failure;
	}
	
	return 0;
}

static void fio_hdfsio_io_u_free(struct thread_data *td, struct io_u *io_u)
{
	struct hdfsio_data *hd = td->io_ops_data;

	if (hd->fs && hdfsDisconnect(hd->fs) < 0) {
		log_err("hdfs: disconnect failed: %d\n", errno);
	}
}

static struct ioengine_ops ioengine_hdfs = {
	.name = "libhdfs",
	.version = FIO_IOOPS_VERSION,
	.flags = FIO_SYNCIO | FIO_DISKLESSIO | FIO_NODISKUTIL,
	.setup = fio_hdfsio_setup,
	.init = fio_hdfsio_init,
	.prep = fio_hdfsio_prep,
	.queue = fio_hdfsio_queue,
	.open_file = fio_hdfsio_open_file,
	.close_file = fio_hdfsio_close_file,
	.io_u_init = fio_hdfsio_io_u_init,
	.io_u_free = fio_hdfsio_io_u_free,
	.option_struct_size	= sizeof(struct hdfsio_options),
	.options		= options,
};


static void fio_init fio_hdfsio_register(void)
{
	register_ioengine(&ioengine_hdfs);
}

static void fio_exit fio_hdfsio_unregister(void)
{
	unregister_ioengine(&ioengine_hdfs);
}
