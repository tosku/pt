#PT
Parallel tempering simulation using Software Transactional Memory spin updates for parallel (multicore) processing in Haskell.

##Local Algorithm: Metropolis


##Build
`stack build`

##Execute
`time stack exec -- pt-exe jobfile.json`

**Currently just pure *Ising* model.**

####jobfile.json
* L: lattice size
* d: dimension
* mcs: Monte Carlo Steps (1 mcs = N $(L^d)$ metropolis spin flip attempts)
* chunks: sampling frequency
* cores: simultaneous spin flip desired
* temp: Temperature
* eqfname: equilibration measurements filename
* resfname: results filename

##Profiling
Building: `stack build -- profile`

Execute with: `stack exec pt-exe jobfile.json -- +RTS -N -p`
and you get profiling file `pt-exe.prof`
