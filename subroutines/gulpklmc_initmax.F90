!
!       Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
!       Affiliation:    University College London
!       Date:           2023.05.25 - 
!
!       Description:    initialising GULP internal counts.
!			See KLMC modified sections in: initial.F90 / modules.F90 (module klmc)
!

subroutine gulpklmc_initmax

  use klmc
  implicit none

  lklmcfreshrun = .true.
  lklmc_maxat            = .true.
  lklmc_maxatloc         = .true.
  lklmc_maxatot          = .true.
  lklmc_maxbond          = .true.
  lklmc_maxbondq         = .true.
  lklmc_maxccspec        = .true.
  lklmc_maxcfg           = .true.
  lklmc_maxconnect       = .true.
  lklmc_maxdef           = .true.
  lklmc_maxeamden        = .true.
  lklmc_maxeamfnspec     = .true.
  lklmc_maxeamspec       = .true.
  lklmc_maxedipspec      = .true.
  lklmc_maxfgrad         = .true.
  lklmc_maxfit           = .true.
  lklmc_maxfor           = .true.
  lklmc_maxfstrain       = .true.
  lklmc_maxgcmcmol       = .true.
  lklmc_maxlambda        = .true.
  lklmc_maxlib           = .true.
  lklmc_maxmcswaps       = .true.
  lklmc_maxmcswapspec    = .true.
  lklmc_maxmctrans       = .true.
  lklmc_maxmol           = .true.
  lklmc_maxnboa          = .true.
  lklmc_maxnboo          = .true.
  lklmc_maxnbopot        = .true.
  lklmc_maxnboq0         = .true.
  lklmc_maxnboq          = .true.
  lklmc_maxnbor          = .true.
  lklmc_maxnboz          = .true.
  lklmc_maxnebreplicatot = .true.
  lklmc_maxnppa          = .true.
  lklmc_maxnpts          = .true.
  lklmc_maxobs           = .true.
  lklmc_maxone           = .true.
  lklmc_maxplanepot      = .true.
  lklmc_maxpot           = .true.
  lklmc_maxqrange        = .true.
  lklmc_maxr1at          = .true.
  lklmc_maxreaxffspec    = .true.
  lklmc_maxreaxffval3    = .true.
  lklmc_maxregion        = .true.
  lklmc_maxsix           = .true.
  lklmc_maxspcellbo      = .true.
  lklmc_maxspcell        = .true.
  lklmc_maxspec          = .true.
  lklmc_maxtdfield       = .true.
  lklmc_maxtempramp      = .true.
  lklmc_maxthb           = .true.
  lklmc_maxtitle         = .true.
  lklmc_maxneighk        = .true.
  lklmc_maxpdfcfg        = .true.

  return
end subroutine
