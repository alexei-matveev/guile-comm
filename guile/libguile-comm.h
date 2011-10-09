//
// These convert between MPI_Comm and SCM object:
//
SCM scm_from_comm (const MPI_Comm comm);
MPI_Comm scm_to_comm (const SCM smob);

//
// This extends interpreter by comm-* primitives,
// calls init_guile_comm_smob() too.
//
void init_guile_comm (void);

//
// In case MPI_Comm is represented by a SMOB call this
// at initialization:
//
void init_guile_comm_smob (void);

