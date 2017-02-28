#PT
Parallel tempering simulation using Software Transactional Memory spin updates for parallel (multicore) processing in Haskell.

##Local Algorithm: Metropolis


##Build
`stack build`

##Execute
`time stack exec -- pt-exe L d mcs  +RTS -N`
####Arguments
**Currently just pure *Ising* model.**
* L: lattice size
* d: dimension
* mcs: Monte Carlo Steps (1 mcs = N metropolis spin flip attempts) 

##Profiling
Building: `stack build -- profile`

Execute with: `stack exec pt-exe 50 2 3000 -- +RTS -N -p`
and you get profiling file `pt-exe.prof`