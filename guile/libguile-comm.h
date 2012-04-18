//
// These convert between MPI_Comm and SCM object:
//
SCM scm_from_comm (const MPI_Comm comm);
MPI_Comm scm_to_comm (const SCM smob);

//
// This extends interpreter by comm-* primitives,
// calls guile_comm_init_smob() too.
//
SCM guile_comm_module_init (void);

//
// In case MPI_Comm is represented by a SMOB call this
// at initialization:
//
void guile_comm_smob_init (void);

