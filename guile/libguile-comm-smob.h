SCM scm_from_comm (const MPI_Comm comm);
MPI_Comm scm_to_comm (const SCM smob);

#define SCM_FROM_COMM(comm) (scm_from_comm (comm))
#define SCM_TO_COMM(smob) (scm_to_comm (smob))
#define SCM_TO_COMM_UNSAFE(smob) (((struct comm_t *) SCM_SMOB_DATA (smob))->comm)

void init_guile_comm_smob (void);
