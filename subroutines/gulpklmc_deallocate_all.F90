!
!       Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
!       Affiliation:    University College London
!       Date:           2023.05.25 - 
!
!       Description:    Cleaning up GULP pointers for next gulpmain calls
!

subroutine gulpklmc_deallocate_all

!
! 24.06.2024 wkjee: deallocate
!
  ! -------------------------------------------------------

  !
  ! subroutine.F90
  ! module what_module_used
  !
  ! # NOTE ---
  ! (1) contents
  !

  !if (associated(p)) then
  !  deallocate(p)
  !  nullify(p)
  !end if

  ! -------------------------------------------------------

  !
  ! find : modules_allocatable_list for the list of pointers used
  ! see, $LIBGULPROOT/Src/_build_libgulp/
  !

  use bondcharge
  use bondorderdata
  use bondvalence
  use bondvectors
  use cellmultipole
  use chargecoupled
  use configurations
  use cosmic
  use cosmicpwtloc
  use current
  use defects
  use derivatives
  use distances
  use dispersion
  use eam               ! not fully done, see related line below : 06.2024 WKJEE
  use EDIPdata
  use eembonds
  use eemdata
  use energies
  use feworkspace
  use field
  use gulp_gfnff
  use fitting
  use four
  use frequencies
  use gaconf
  use general
  use genetic
  use gulpinput
  use ksample
  use ksample_scatter
  use kspace
  use library
  use montecarlo
  use moldyn
  use molecule
  use g_neb
  use observables
  use one
  use optimisation
  use parallel
  use partial
  use phononatoms
  use plane
  use polarise
  use potchange
  use potentialgrid
  use potentialinterpolation
  use potentialpoints
  use potentialsites
  use potentialxyz
  use projectdos
  use properties
  use radial
  use reaxFFdata
  use realvectors
  use region2a
  use scan
  use scatterdata
  use shells
  use shellextrapolation
  use shifts
  use six
  use spatial
  use spatialbo
  use species
  use splinedata
  use spme
  use sutton
  use symmetry
  use thermalcond
  use m_three
  use m_ti
  use transform
  use two
  use freeze
  use uffdata
  use vectors             ! not fully done, see related line below : 06.2024 WKJEE
  use velocities
  use xcgc

  !
  ! additional modules
  !
  use m_gfnff_nbr3
  use kim_models
  use m_pr
  use m_pdfneutron

  implicit none

  !
  ! module bondcharge
  !
  if (associated(symbolbondQ)) then
    deallocate(symbolbondQ)
    nullify(symbolbondQ)
  end if
  if (associated(nbondQspec1)) then
    deallocate(nbondQspec1)
    nullify(nbondQspec1)
  end if
  if (associated(nbondQspec2)) then
    deallocate(nbondQspec2)
    nullify(nbondQspec2)
  end if
  if (associated(nbondQtyp1)) then
    deallocate(nbondQtyp1)
    nullify(nbondQtyp1)
  end if
  if (associated(nbondQtyp2)) then
    deallocate(nbondQtyp2)
    nullify(nbondQtyp2)
  end if
  if (associated(bondQincrement)) then
    deallocate(bondQincrement)
    nullify(bondQincrement)
  end if
  
  !
  ! module bondorderdata
  !
  if (associated(nBOspec0)) then
    deallocate(nBOspec0)
    nullify(nBOspec0)
  end if
  if (associated(nBOspec1)) then
    deallocate(nBOspec1)
    nullify(nBOspec1)
  end if
  if (associated(nBOspec2)) then
    deallocate(nBOspec2)
    nullify(nBOspec2)
  end if
  if (associated(nBOspecA1)) then
    deallocate(nBOspecA1)
    nullify(nBOspecA1)
  end if
  if (associated(nBOspecA2)) then
    deallocate(nBOspecA2)
    nullify(nBOspecA2)
  end if
  if (associated(nBOspecR1)) then
    deallocate(nBOspecR1)
    nullify(nBOspecR1)
  end if
  if (associated(nBOspecR2)) then
    deallocate(nBOspecR2)
    nullify(nBOspecR2)
  end if
  if (associated(nBOspecQ0)) then
    deallocate(nBOspecQ0)
    nullify(nBOspecQ0)
  end if
  if (associated(nBOspecQ1)) then
    deallocate(nBOspecQ1)
    nullify(nBOspecQ1)
  end if
  if (associated(nBOspecQ2)) then
    deallocate(nBOspecQ2)
    nullify(nBOspecQ2)
  end if
  if (associated(nBOspecZ)) then
    deallocate(nBOspecZ)
    nullify(nBOspecZ)
  end if
  if (associated(nBOtaperQ)) then
    deallocate(nBOtaperQ)
    nullify(nBOtaperQ)
  end if
  if (associated(nBOtapertype)) then
    deallocate(nBOtapertype)
    nullify(nBOtapertype)
  end if
  if (associated(nBOtyp0)) then
    deallocate(nBOtyp0)
    nullify(nBOtyp0)
  end if
  if (associated(nBOtyp1)) then
    deallocate(nBOtyp1)
    nullify(nBOtyp1)
  end if
  if (associated(nBOtyp2)) then
    deallocate(nBOtyp2)
    nullify(nBOtyp2)
  end if
  if (associated(nBOtypA1)) then
    deallocate(nBOtypA1)
    nullify(nBOtypA1)
  end if
  if (associated(nBOtypA2)) then
    deallocate(nBOtypA2)
    nullify(nBOtypA2)
  end if
  if (associated(nBOtypR1)) then
    deallocate(nBOtypR1)
    nullify(nBOtypR1)
  end if
  if (associated(nBOtypR2)) then
    deallocate(nBOtypR2)
    nullify(nBOtypR2)
  end if
  if (associated(nBOtypQ0)) then
    deallocate(nBOtypQ0)
    nullify(nBOtypQ0)
  end if
  if (associated(nBOtypQ1)) then
    deallocate(nBOtypQ1)
    nullify(nBOtypQ1)
  end if
  if (associated(nBOtypQ2)) then
    deallocate(nBOtypQ2)
    nullify(nBOtypQ2)
  end if
  if (associated(nBOtypZ)) then
    deallocate(nBOtypZ)
    nullify(nBOtypZ)
  end if
  if (associated(nBOtypeA)) then
    deallocate(nBOtypeA)
    nullify(nBOtypeA)
  end if
  if (associated(nBOtypeR)) then
    deallocate(nBOtypeR)
    nullify(nBOtypeR)
  end if
  if (associated(nBOtypeQ)) then
    deallocate(nBOtypeQ)
    nullify(nBOtypeQ)
  end if
  if (associated(nBOtypeQ0)) then
    deallocate(nBOtypeQ0)
    nullify(nBOtypeQ0)
  end if
  if (associated(nBOtypeT)) then
    deallocate(nBOtypeT)
    nullify(nBOtypeT)
  end if
  if (associated(BOcombi)) then
    deallocate(BOcombi)
    nullify(BOcombi)
  end if
  if (associated(lBOgikA)) then
    deallocate(lBOgikA)
    nullify(lBOgikA)
  end if
  if (associated(lBOgikR)) then
    deallocate(lBOgikR)
    nullify(lBOgikR)
  end if
  if (associated(lBOsymA)) then
    deallocate(lBOsymA)
    nullify(lBOsymA)
  end if
  if (associated(lBOsymR)) then
    deallocate(lBOsymR)
    nullify(lBOsymR)
  end if
  if (associated(lBOzrlA)) then
    deallocate(lBOzrlA)
    nullify(lBOzrlA)
  end if
  if (associated(lBOzrlR)) then
    deallocate(lBOzrlR)
    nullify(lBOzrlR)
  end if
  if (associated(BOacoeff)) then
    deallocate(BOacoeff)
    nullify(BOacoeff)
  end if
  if (associated(BObcoeff)) then
    deallocate(BObcoeff)
    nullify(BObcoeff)
  end if
  if (associated(BOccoeffA)) then
    deallocate(BOccoeffA)
    nullify(BOccoeffA)
  end if
  if (associated(BOccoeffR)) then
    deallocate(BOccoeffR)
    nullify(BOccoeffR)
  end if
  if (associated(BOccoeffZ)) then
    deallocate(BOccoeffZ)
    nullify(BOccoeffZ)
  end if
  if (associated(BOchiA)) then
    deallocate(BOchiA)
    nullify(BOchiA)
  end if
  if (associated(BOchiR)) then
    deallocate(BOchiR)
    nullify(BOchiR)
  end if
  if (associated(BOecoeffA)) then
    deallocate(BOecoeffA)
    nullify(BOecoeffA)
  end if
  if (associated(BOecoeffR)) then
    deallocate(BOecoeffR)
    nullify(BOecoeffR)
  end if
  if (associated(BOecoeffZ)) then
    deallocate(BOecoeffZ)
    nullify(BOecoeffZ)
  end if
  if (associated(BOhcoeffA)) then
    deallocate(BOhcoeffA)
    nullify(BOhcoeffA)
  end if
  if (associated(BOhcoeffR)) then
    deallocate(BOhcoeffR)
    nullify(BOhcoeffR)
  end if
  if (associated(BOlcoeffA)) then
    deallocate(BOlcoeffA)
    nullify(BOlcoeffA)
  end if
  if (associated(BOlcoeffR)) then
    deallocate(BOlcoeffR)
    nullify(BOlcoeffR)
  end if
  if (associated(BOmcoeffA)) then
    deallocate(BOmcoeffA)
    nullify(BOmcoeffA)
  end if
  if (associated(BOmcoeffR)) then
    deallocate(BOmcoeffR)
    nullify(BOmcoeffR)
  end if
  if (associated(BOncoeffA)) then
    deallocate(BOncoeffA)
    nullify(BOncoeffA)
  end if
  if (associated(BOncoeffR)) then
    deallocate(BOncoeffR)
    nullify(BOncoeffR)
  end if
  if (associated(BOocoeffA)) then
    deallocate(BOocoeffA)
    nullify(BOocoeffA)
  end if
  if (associated(BOocoeffR)) then
    deallocate(BOocoeffR)
    nullify(BOocoeffR)
  end if
  if (associated(BOzcoeffZ)) then
    deallocate(BOzcoeffZ)
    nullify(BOzcoeffZ)
  end if
  if (associated(BOq0)) then
    deallocate(BOq0)
    nullify(BOq0)
  end if
  if (associated(BOq0pot)) then
    deallocate(BOq0pot)
    nullify(BOq0pot)
  end if
  if (associated(BOq0ref)) then
    deallocate(BOq0ref)
    nullify(BOq0ref)
  end if
  if (associated(BOq0rho)) then
    deallocate(BOq0rho)
    nullify(BOq0rho)
  end if
  if (associated(BOzacoeff)) then
    deallocate(BOzacoeff)
    nullify(BOzacoeff)
  end if
  if (associated(BOzbcoeff)) then
    deallocate(BOzbcoeff)
    nullify(BOzbcoeff)
  end if
  if (associated(rBOmax)) then
    deallocate(rBOmax)
    nullify(rBOmax)
  end if
  if (associated(rBOmin)) then
    deallocate(rBOmin)
    nullify(rBOmin)
  end if
  if (associated(rBOmaxQ)) then
    deallocate(rBOmaxQ)
    nullify(rBOmaxQ)
  end if
  if (associated(rBOminQ)) then
    deallocate(rBOminQ)
    nullify(rBOminQ)
  end if

  !
  ! module bondvalence
  !
  if (associated(nVBspecE)) then
    deallocate(nVBspecE)
    nullify(nVBspecE)
  end if
  if (associated(nVBspecB1)) then
    deallocate(nVBspecB1)
    nullify(nVBspecB1)
  end if
  if (associated(nVBspecB2)) then
    deallocate(nVBspecB2)
    nullify(nVBspecB2)
  end if
  if (associated(nVBtypeE)) then
    deallocate(nVBtypeE)
    nullify(nVBtypeE)
  end if
  if (associated(nVBtypeB1)) then
    deallocate(nVBtypeB1)
    nullify(nVBtypeB1)
  end if
  if (associated(nVBtypeB2)) then
    deallocate(nVBtypeB2)
    nullify(nVBtypeB2)
  end if
  if (associated(nvalbondtype)) then
    deallocate(nvalbondtype)
    nullify(nvalbondtype)
  end if
  if (associated(nvalenertype)) then
    deallocate(nvalenertype)
    nullify(nvalenertype)
  end if
  if (associated(VBparB)) then
    deallocate(VBparB)
    nullify(VBparB)
  end if
  if (associated(VBparE)) then
    deallocate(VBparE)
    nullify(VBparE)
  end if
  if (associated(VBwgtB)) then
    deallocate(VBwgtB)
    nullify(VBwgtB)
  end if
  if (associated(rVBmax)) then
    deallocate(rVBmax)
    nullify(rVBmax)
  end if
  if (associated(rVBmin)) then
    deallocate(rVBmin)
    nullify(rVBmin)
  end if

  !
  ! module bondvectors
  !
  if (associated(nbondvecind)) then
    deallocate(nbondvecind)
    nullify(nbondvecind)
  end if
  if (associated(nbtypevec)) then
    deallocate(nbtypevec)
    nullify(nbtypevec)
  end if
  if (associated(nbtype2vec)) then
    deallocate(nbtype2vec)
    nullify(nbtype2vec)
  end if
  if (associated(lbondedvec)) then
    deallocate(lbondedvec)
    nullify(lbondedvec)
  end if
  if (associated(l2bondsvec)) then
    deallocate(l2bondsvec)
    nullify(l2bondsvec)
  end if
  if (associated(l3bondsvec)) then
    deallocate(l3bondsvec)
    nullify(l3bondsvec)
  end if

  !
  ! module cellmultipole
  !
  if (associated(nboxat)) then
    deallocate(nboxat)
    nullify(nboxat)
  end if

  !
  ! module chargecoupled
  !
  if (associated(natCCspec)) then
    deallocate(natCCspec)
    nullify(natCCspec)
  end if
  if (associated(ntypCCspec)) then
    deallocate(ntypCCspec)
    nullify(ntypCCspec)
  end if
  if (associated(nCCparNb)) then
    deallocate(nCCparNb)
    nullify(nCCparNb)
  end if
  if (associated(CCbeta)) then
    deallocate(CCbeta)
    nullify(CCbeta)
  end if
  if (associated(CCeta)) then
    deallocate(CCeta)
    nullify(CCeta)
  end if
  if (associated(CClambda)) then
    deallocate(CClambda)
    nullify(CClambda)
  end if
  if (associated(CCmu)) then
    deallocate(CCmu)
    nullify(CCmu)
  end if
  if (associated(CCparA)) then
    deallocate(CCparA)
    nullify(CCparA)
  end if
  if (associated(CCparAE)) then
    deallocate(CCparAE)
    nullify(CCparAE)
  end if
  if (associated(CCparB)) then
    deallocate(CCparB)
    nullify(CCparB)
  end if
  if (associated(CCparC)) then
    deallocate(CCparC)
    nullify(CCparC)
  end if
  if (associated(CCparD)) then
    deallocate(CCparD)
    nullify(CCparD)
  end if
  if (associated(CCparDL)) then
    deallocate(CCparDL)
    nullify(CCparDL)
  end if
  if (associated(CCparDU)) then
    deallocate(CCparDU)
    nullify(CCparDU)
  end if
  if (associated(CCparH)) then
    deallocate(CCparH)
    nullify(CCparH)
  end if
  if (associated(CCparM)) then
    deallocate(CCparM)
    nullify(CCparM)
  end if
  if (associated(CCparN)) then
    deallocate(CCparN)
    nullify(CCparN)
  end if
  if (associated(CCparIE)) then
    deallocate(CCparIE)
    nullify(CCparIE)
  end if
  if (associated(CCparQL)) then
    deallocate(CCparQL)
    nullify(CCparQL)
  end if
  if (associated(CCparQU)) then
    deallocate(CCparQU)
    nullify(CCparQU)
  end if
  if (associated(CCvdwC)) then
    deallocate(CCvdwC)
    nullify(CCvdwC)
  end if
  if (associated(rCCmaxL)) then
    deallocate(rCCmaxL)
    nullify(rCCmaxL)
  end if
  if (associated(rCCmaxS)) then
    deallocate(rCCmaxS)
    nullify(rCCmaxS)
  end if
  if (associated(rCCminL)) then
    deallocate(rCCminL)
    nullify(rCCminL)
  end if
  if (associated(rCCminS)) then
    deallocate(rCCminS)
    nullify(rCCminS)
  end if

  !
  ! module configurations
  !
  if (associated(names)) then
    deallocate(names)
    nullify(names)
  end if
  if (associated(ioptindexcfg)) then
    deallocate(ioptindexcfg)
    nullify(ioptindexcfg)
  end if
  if (associated(iopttypecfg)) then
    deallocate(iopttypecfg)
    nullify(iopttypecfg)
  end if
  if (associated(maxmodecfg)) then
    deallocate(maxmodecfg)
    nullify(maxmodecfg)
  end if
  if (associated(minmodecfg)) then
    deallocate(minmodecfg)
    nullify(minmodecfg)
  end if
  if (associated(n1con)) then
    deallocate(n1con)
    nullify(n1con)
  end if
  if (associated(n1var)) then
    deallocate(n1var)
    nullify(n1var)
  end if
  if (associated(nd2cellcfg)) then
    deallocate(nd2cellcfg)
    nullify(nd2cellcfg)
  end if
  if (associated(ndimen)) then
    deallocate(ndimen)
    nullify(ndimen)
  end if
  if (associated(nascfg)) then
    deallocate(nascfg)
    nullify(nascfg)
  end if
  if (associated(natcfg)) then
    deallocate(natcfg)
    nullify(natcfg)
  end if
  if (associated(nbornstep)) then
    deallocate(nbornstep)
    nullify(nbornstep)
  end if
  if (associated(ncellmaxcfg)) then
    deallocate(ncellmaxcfg)
    nullify(ncellmaxcfg)
  end if
  if (associated(ncellmincfg)) then
    deallocate(ncellmincfg)
    nullify(ncellmincfg)
  end if
  if (associated(ncfixindcfg)) then
    deallocate(ncfixindcfg)
    nullify(ncfixindcfg)
  end if
  if (associated(ncfixtypcfg)) then
    deallocate(ncfixtypcfg)
    nullify(ncfixtypcfg)
  end if
  if (associated(nconcfg)) then
    deallocate(nconcfg)
    nullify(nconcfg)
  end if
  if (associated(ncorepcfg)) then
    deallocate(ncorepcfg)
    nullify(ncorepcfg)
  end if
  if (associated(ncvarindcfg)) then
    deallocate(ncvarindcfg)
    nullify(ncvarindcfg)
  end if
  if (associated(ncvartypcfg)) then
    deallocate(ncvartypcfg)
    nullify(ncvartypcfg)
  end if
  if (associated(neiglow)) then
    deallocate(neiglow)
    nullify(neiglow)
  end if
  if (associated(neighigh)) then
    deallocate(neighigh)
    nullify(neighigh)
  end if
  if (associated(nfixatomtypecfg)) then
    deallocate(nfixatomtypecfg)
    nullify(nfixatomtypecfg)
  end if
  if (associated(nfixatomcfg)) then
    deallocate(nfixatomcfg)
    nullify(nfixatomcfg)
  end if
  if (associated(ninternalmaxcfg)) then
    deallocate(ninternalmaxcfg)
    nullify(ninternalmaxcfg)
  end if
  if (associated(ninternalmincfg)) then
    deallocate(ninternalmincfg)
    nullify(ninternalmincfg)
  end if
  if (associated(nomegastep)) then
    deallocate(nomegastep)
    nullify(nomegastep)
  end if
  if (associated(nregionno)) then
    deallocate(nregionno)
    nullify(nregionno)
  end if
  if (associated(nregions)) then
    deallocate(nregions)
    nullify(nregions)
  end if
  if (associated(nregiontype)) then
    deallocate(nregiontype)
    nullify(nregiontype)
  end if
  if (associated(nspecptrcfg)) then
    deallocate(nspecptrcfg)
    nullify(nspecptrcfg)
  end if
  if (associated(nsregion2)) then
    deallocate(nsregion2)
    nullify(nsregion2)
  end if
  if (associated(nsuper)) then
    deallocate(nsuper)
    nullify(nsuper)
  end if
  if (associated(nsuperghost)) then
    deallocate(nsuperghost)
    nullify(nsuperghost)
  end if
  if (associated(ntempramp)) then
    deallocate(ntempramp)
    nullify(ntempramp)
  end if
  if (associated(ntempstp)) then
    deallocate(ntempstp)
    nullify(ntempstp)
  end if
  if (associated(ntempstpstart)) then
    deallocate(ntempstpstart)
    nullify(ntempstpstart)
  end if
  if (associated(ntypcfg)) then
    deallocate(ntypcfg)
    nullify(ntypcfg)
  end if
  if (associated(ntwistcfg)) then
    deallocate(ntwistcfg)
    nullify(ntwistcfg)
  end if
  if (associated(nummodecfg)) then
    deallocate(nummodecfg)
    nullify(nummodecfg)
  end if
  if (associated(nvarcfg)) then
    deallocate(nvarcfg)
    nullify(nvarcfg)
  end if
  if (associated(nzmolcfg)) then
    deallocate(nzmolcfg)
    nullify(nzmolcfg)
  end if
  if (associated(omegadirtype)) then
    deallocate(omegadirtype)
    nullify(omegadirtype)
  end if
  if (associated(ordersuper)) then
    deallocate(ordersuper)
    nullify(ordersuper)
  end if
  if (associated(QMMMmode)) then
    deallocate(QMMMmode)
    nullify(QMMMmode)
  end if
  if (associated(ramandirtype)) then
    deallocate(ramandirtype)
    nullify(ramandirtype)
  end if
  if (associated(lanisotropicpresscfg)) then
    deallocate(lanisotropicpresscfg)
    nullify(lanisotropicpresscfg)
  end if
  if (associated(lbornkin)) then
    deallocate(lbornkin)
    nullify(lbornkin)
  end if
  if (associated(lbsmat)) then
    deallocate(lbsmat)
    nullify(lbsmat)
  end if
  if (associated(leinsteinat)) then
    deallocate(leinsteinat)
    nullify(leinsteinat)
  end if
  if (associated(ltibeinsteinat)) then
    deallocate(ltibeinsteinat)
    nullify(ltibeinsteinat)
  end if
  if (associated(ltifeinsteinat)) then
    deallocate(ltifeinsteinat)
    nullify(ltifeinsteinat)
  end if
  if (associated(lfcborn)) then
    deallocate(lfcborn)
    nullify(lfcborn)
  end if
  if (associated(lfcphon)) then
    deallocate(lfcphon)
    nullify(lfcphon)
  end if
  if (associated(lfcprop)) then
    deallocate(lfcprop)
    nullify(lfcprop)
  end if
  if (associated(lfcscatter)) then
    deallocate(lfcscatter)
    nullify(lfcscatter)
  end if
  if (associated(linitcfg)) then
    deallocate(linitcfg)
    nullify(linitcfg)
  end if
  if (associated(llowered)) then
    deallocate(llowered)
    nullify(llowered)
  end if
  if (associated(lomega)) then
    deallocate(lomega)
    nullify(lomega)
  end if
  if (associated(lopfc)) then
    deallocate(lopfc)
    nullify(lopfc)
  end if
  if (associated(lopfi)) then
    deallocate(lopfi)
    nullify(lopfi)
  end if
  if (associated(lopfreg)) then
    deallocate(lopfreg)
    nullify(lopfreg)
  end if
  if (associated(lqmatom)) then
    deallocate(lqmatom)
    nullify(lqmatom)
  end if
  if (associated(lregionrigid)) then
    deallocate(lregionrigid)
    nullify(lregionrigid)
  end if
  if (associated(lshearforcecfg)) then
    deallocate(lshearforcecfg)
    nullify(lshearforcecfg)
  end if
  if (associated(lsliceatom)) then
    deallocate(lsliceatom)
    nullify(lsliceatom)
  end if
  if (associated(ltdforcecfg)) then
    deallocate(ltdforcecfg)
    nullify(ltdforcecfg)
  end if
  if (associated(lvecin)) then
    deallocate(lvecin)
    nullify(lvecin)
  end if
  if (associated(lvecpin)) then
    deallocate(lvecpin)
    nullify(lvecpin)
  end if
  if (associated(anisotropicpresscfg)) then
    deallocate(anisotropicpresscfg)
    nullify(anisotropicpresscfg)
  end if
  if (associated(bornk)) then
    deallocate(bornk)
    nullify(bornk)
  end if
  if (associated(cncfg)) then
    deallocate(cncfg)
    nullify(cncfg)
  end if
  if (associated(conaddcfg)) then
    deallocate(conaddcfg)
    nullify(conaddcfg)
  end if
  if (associated(concocfg)) then
    deallocate(concocfg)
    nullify(concocfg)
  end if
  if (associated(dhklcfg)) then
    deallocate(dhklcfg)
    nullify(dhklcfg)
  end if
  if (associated(dielectriccfg)) then
    deallocate(dielectriccfg)
    nullify(dielectriccfg)
  end if
  if (associated(energycfg)) then
    deallocate(energycfg)
    nullify(energycfg)
  end if
  if (associated(extpotcfg)) then
    deallocate(extpotcfg)
    nullify(extpotcfg)
  end if
  if (associated(forcecfg)) then
    deallocate(forcecfg)
    nullify(forcecfg)
  end if
  if (associated(keinsteinat)) then
    deallocate(keinsteinat)
    nullify(keinsteinat)
  end if
  if (associated(occucfg)) then
    deallocate(occucfg)
    nullify(occucfg)
  end if
  if (associated(omega)) then
    deallocate(omega)
    nullify(omega)
  end if
  if (associated(omegadamping)) then
    deallocate(omegadamping)
    nullify(omegadamping)
  end if
  if (associated(omegadir)) then
    deallocate(omegadir)
    nullify(omegadir)
  end if
  if (associated(omegastep)) then
    deallocate(omegastep)
    nullify(omegastep)
  end if
  if (associated(oxcfg)) then
    deallocate(oxcfg)
    nullify(oxcfg)
  end if
  if (associated(qlcfg)) then
    deallocate(qlcfg)
    nullify(qlcfg)
  end if
  if (associated(presscfg)) then
    deallocate(presscfg)
    nullify(presscfg)
  end if
  if (associated(radcfg)) then
    deallocate(radcfg)
    nullify(radcfg)
  end if
  if (associated(ramandir)) then
    deallocate(ramandir)
    nullify(ramandir)
  end if
  if (associated(rvcfg)) then
    deallocate(rvcfg)
    nullify(rvcfg)
  end if
  if (associated(rvpcfg)) then
    deallocate(rvpcfg)
    nullify(rvpcfg)
  end if
  if (associated(sbulkecfg)) then
    deallocate(sbulkecfg)
    nullify(sbulkecfg)
  end if
  if (associated(shearforcecfg)) then
    deallocate(shearforcecfg)
    nullify(shearforcecfg)
  end if
  if (associated(shearforcedircfg)) then
    deallocate(shearforcedircfg)
    nullify(shearforcedircfg)
  end if
  if (associated(shearforcenormcfg)) then
    deallocate(shearforcenormcfg)
    nullify(shearforcenormcfg)
  end if
  if (associated(straincfg)) then
    deallocate(straincfg)
    nullify(straincfg)
  end if
  if (associated(stresscfg)) then
    deallocate(stresscfg)
    nullify(stresscfg)
  end if
  if (associated(tdforcecfg)) then
    deallocate(tdforcecfg)
    nullify(tdforcecfg)
  end if
  if (associated(tempcfg)) then
    deallocate(tempcfg)
    nullify(tempcfg)
  end if
  if (associated(tempstp)) then
    deallocate(tempstp)
    nullify(tempstp)
  end if
  if (associated(totalchargecfg)) then
    deallocate(totalchargecfg)
    nullify(totalchargecfg)
  end if
  if (associated(xcfg)) then
    deallocate(xcfg)
    nullify(xcfg)
  end if
  if (associated(ycfg)) then
    deallocate(ycfg)
    nullify(ycfg)
  end if
  if (associated(zcfg)) then
    deallocate(zcfg)
    nullify(zcfg)
  end if
  if (associated(xinitcfg)) then
    deallocate(xinitcfg)
    nullify(xinitcfg)
  end if
  if (associated(yinitcfg)) then
    deallocate(yinitcfg)
    nullify(yinitcfg)
  end if
  if (associated(zinitcfg)) then
    deallocate(zinitcfg)
    nullify(zinitcfg)
  end if
  if (associated(xceinsteinat)) then
    deallocate(xceinsteinat)
    nullify(xceinsteinat)
  end if
  if (associated(yceinsteinat)) then
    deallocate(yceinsteinat)
    nullify(yceinsteinat)
  end if
  if (associated(zceinsteinat)) then
    deallocate(zceinsteinat)
    nullify(zceinsteinat)
  end if
  if (associated(xfeinsteinat)) then
    deallocate(xfeinsteinat)
    nullify(xfeinsteinat)
  end if
  if (associated(yfeinsteinat)) then
    deallocate(yfeinsteinat)
    nullify(yfeinsteinat)
  end if
  if (associated(zfeinsteinat)) then
    deallocate(zfeinsteinat)
    nullify(zfeinsteinat)
  end if
  if (associated(lgfnff_ref_rv)) then
    deallocate(lgfnff_ref_rv)
    nullify(lgfnff_ref_rv)
  end if
  if (associated(lgfnff_ref_crd)) then
    deallocate(lgfnff_ref_crd)
    nullify(lgfnff_ref_crd)
  end if
  if (associated(gfnff_ref_rv)) then
    deallocate(gfnff_ref_rv)
    nullify(gfnff_ref_rv)
  end if
  if (associated(gfnff_ref_crd)) then
    deallocate(gfnff_ref_crd)
    nullify(gfnff_ref_crd)
  end if
  if (associated(gfnff_ref_q)) then
    deallocate(gfnff_ref_q)
    nullify(gfnff_ref_q)
  end if


  !
  ! module cosmic
  !
  if (associated(cosmoatomptr)) then
    deallocate(cosmoatomptr)
    nullify(cosmoatomptr)
  end if
  if (associated(nar)) then
    deallocate(nar)
    nullify(nar)
  end if
  if (associated(nallnearsegptr)) then
    deallocate(nallnearsegptr)
    nullify(nallnearsegptr)
  end if
  if (associated(nallnearsegrptr)) then
    deallocate(nallnearsegrptr)
    nullify(nallnearsegrptr)
  end if
  if (associated(nnearseg)) then
    deallocate(nnearseg)
    nullify(nnearseg)
  end if
  if (associated(nnearsegptr)) then
    deallocate(nnearsegptr)
    nullify(nnearsegptr)
  end if
  if (associated(nnearsegptrcell)) then
    deallocate(nnearsegptrcell)
    nullify(nnearsegptrcell)
  end if
  if (associated(npwt)) then
    deallocate(npwt)
    nullify(npwt)
  end if
  if (associated(npwtptr)) then
    deallocate(npwtptr)
    nullify(npwtptr)
  end if
  if (associated(nsasexcludemax)) then
    deallocate(nsasexcludemax)
    nullify(nsasexcludemax)
  end if
  if (associated(nsasexcludemin)) then
    deallocate(nsasexcludemin)
    nullify(nsasexcludemin)
  end if
  if (associated(nsasparticleptr)) then
    deallocate(nsasparticleptr)
    nullify(nsasparticleptr)
  end if
  if (associated(nsasparticlepartptr)) then
    deallocate(nsasparticlepartptr)
    nullify(nsasparticlepartptr)
  end if
  if (associated(nset)) then
    deallocate(nset)
    nullify(nset)
  end if
  if (associated(nsetf)) then
    deallocate(nsetf)
    nullify(nsetf)
  end if
  if (associated(lcosmoeigin)) then
    deallocate(lcosmoeigin)
    nullify(lcosmoeigin)
  end if
  if (associated(atsrad)) then
    deallocate(atsrad)
    nullify(atsrad)
  end if
  if (associated(cosmoA)) then
    deallocate(cosmoA)
    nullify(cosmoA)
  end if
  if (associated(cosmoBq)) then
    deallocate(cosmoBq)
    nullify(cosmoBq)
  end if
  if (associated(cosmoeigen)) then
    deallocate(cosmoeigen)
    nullify(cosmoeigen)
  end if
  if (associated(cosmoepsilon)) then
    deallocate(cosmoepsilon)
    nullify(cosmoepsilon)
  end if
  if (associated(cosmodrsolv)) then
    deallocate(cosmodrsolv)
    nullify(cosmodrsolv)
  end if
  if (associated(cosmorsolv)) then
    deallocate(cosmorsolv)
    nullify(cosmorsolv)
  end if
  if (associated(cosmotm)) then
    deallocate(cosmotm)
    nullify(cosmotm)
  end if
  if (associated(cosmopwt)) then
    deallocate(cosmopwt)
    nullify(cosmopwt)
  end if
  if (associated(cosmowt)) then
    deallocate(cosmowt)
    nullify(cosmowt)
  end if
  if (associated(qonsas)) then
    deallocate(qonsas)
    nullify(qonsas)
  end if
  if (associated(qonsastarget)) then
    deallocate(qonsastarget)
    nullify(qonsastarget)
  end if
  if (associated(qsasparticles)) then
    deallocate(qsasparticles)
    nullify(qsasparticles)
  end if
  if (associated(sas)) then
    deallocate(sas)
    nullify(sas)
  end if
  if (associated(segweight)) then
    deallocate(segweight)
    nullify(segweight)
  end if
  if (associated(sphere1h)) then
    deallocate(sphere1h)
    nullify(sphere1h)
  end if
  if (associated(sphere1)) then
    deallocate(sphere1)
    nullify(sphere1)
  end if
  if (associated(sphere2)) then
    deallocate(sphere2)
    nullify(sphere2)
  end if
  if (associated(spxyz)) then
    deallocate(spxyz)
    nullify(spxyz)
  end if
  if (associated(spxyzouter)) then
    deallocate(spxyzouter)
    nullify(spxyzouter)
  end if

  !
  ! module cosmicpwtloc
  !
  if (associated(npwtloc)) then
    deallocate(npwtloc)
    nullify(npwtloc)
  end if
  if (associated(npwtptrloc)) then
    deallocate(npwtptrloc)
    nullify(npwtptrloc)
  end if

  !
  ! module current
  !
  if (associated(nbonds)) then
    deallocate(nbonds)
    nullify(nbonds)
  end if
  if (associated(nbonded)) then
    deallocate(nbonded)
    nullify(nbonded)
  end if
  if (associated(nbondedtype)) then
    deallocate(nbondedtype)
    nullify(nbondedtype)
  end if
  if (associated(nbondind)) then
    deallocate(nbondind)
    nullify(nbondind)
  end if
  if (associated(nbondqb)) then
    deallocate(nbondqb)
    nullify(nbondqb)
  end if
  if (associated(icosx)) then
    deallocate(icosx)
    nullify(icosx)
  end if
  if (associated(icosy)) then
    deallocate(icosy)
    nullify(icosy)
  end if
  if (associated(icosz)) then
    deallocate(icosz)
    nullify(icosz)
  end if
  if (associated(icosxsp)) then
    deallocate(icosxsp)
    nullify(icosxsp)
  end if
  if (associated(icosysp)) then
    deallocate(icosysp)
    nullify(icosysp)
  end if
  if (associated(icoszsp)) then
    deallocate(icoszsp)
    nullify(icoszsp)
  end if
  if (associated(ioptindex)) then
    deallocate(ioptindex)
    nullify(ioptindex)
  end if
  if (associated(iopttype)) then
    deallocate(iopttype)
    nullify(iopttype)
  end if
  if (associated(iatn)) then
    deallocate(iatn)
    nullify(iatn)
  end if
  if (associated(nasymnomolptr)) then
    deallocate(nasymnomolptr)
    nullify(nasymnomolptr)
  end if
  if (associated(nasymnomolrptr)) then
    deallocate(nasymnomolrptr)
    nullify(nasymnomolrptr)
  end if
  if (associated(ncorenomolptr)) then
    deallocate(ncorenomolptr)
    nullify(ncorenomolptr)
  end if
  if (associated(ncorenomolrptr)) then
    deallocate(ncorenomolrptr)
    nullify(ncorenomolrptr)
  end if
  if (associated(numatnomolonnodeptr)) then
    deallocate(numatnomolonnodeptr)
    nullify(numatnomolonnodeptr)
  end if
  if (associated(numatnomolptr)) then
    deallocate(numatnomolptr)
    nullify(numatnomolptr)
  end if
  if (associated(numatnomolrptr)) then
    deallocate(numatnomolrptr)
    nullify(numatnomolrptr)
  end if
  if (associated(nat)) then
    deallocate(nat)
    nullify(nat)
  end if
  if (associated(natype)) then
    deallocate(natype)
    nullify(natype)
  end if
  if (associated(nftype)) then
    deallocate(nftype)
    nullify(nftype)
  end if
  if (associated(nchargeptr)) then
    deallocate(nchargeptr)
    nullify(nchargeptr)
  end if
  if (associated(nchargec6ptr)) then
    deallocate(nchargec6ptr)
    nullify(nchargec6ptr)
  end if
  if (associated(ncfixind)) then
    deallocate(ncfixind)
    nullify(ncfixind)
  end if
  if (associated(ncfixtyp)) then
    deallocate(ncfixtyp)
    nullify(ncfixtyp)
  end if
  if (associated(ncvarind)) then
    deallocate(ncvarind)
    nullify(ncvarind)
  end if
  if (associated(ncvartyp)) then
    deallocate(ncvartyp)
    nullify(ncvartyp)
  end if
  if (associated(neamfnspecptr)) then
    deallocate(neamfnspecptr)
    nullify(neamfnspecptr)
  end if
  if (associated(neamspecptr)) then
    deallocate(neamspecptr)
    nullify(neamspecptr)
  end if
  if (associated(neemptr)) then
    deallocate(neemptr)
    nullify(neemptr)
  end if
  if (associated(neemrptr)) then
    deallocate(neemrptr)
    nullify(neemrptr)
  end if
  if (associated(neemlocptr)) then
    deallocate(neemlocptr)
    nullify(neemlocptr)
  end if
  if (associated(neemlocrptr)) then
    deallocate(neemlocrptr)
    nullify(neemlocrptr)
  end if
  if (associated(neqv)) then
    deallocate(neqv)
    nullify(neqv)
  end if
  if (associated(nrela2f)) then
    deallocate(nrela2f)
    nullify(nrela2f)
  end if
  if (associated(nrelf2a)) then
    deallocate(nrelf2a)
    nullify(nrelf2a)
  end if
  if (associated(nmolrela2f)) then
    deallocate(nmolrela2f)
    nullify(nmolrela2f)
  end if
  if (associated(nmolrelf2a)) then
    deallocate(nmolrelf2a)
    nullify(nmolrelf2a)
  end if
  if (associated(nrotop)) then
    deallocate(nrotop)
    nullify(nrotop)
  end if
  if (associated(nspecptr)) then
    deallocate(nspecptr)
    nullify(nspecptr)
  end if
  if (associated(ntemperaturestep)) then
    deallocate(ntemperaturestep)
    nullify(ntemperaturestep)
  end if
  if (associated(ntemperaturestepstart)) then
    deallocate(ntemperaturestepstart)
    nullify(ntemperaturestepstart)
  end if
  if (associated(ntemperaturestepstop)) then
    deallocate(ntemperaturestepstop)
    nullify(ntemperaturestepstop)
  end if
  if (associated(bornq)) then
    deallocate(bornq)
    nullify(bornq)
  end if
  if (associated(c6a)) then
    deallocate(c6a)
    nullify(c6a)
  end if
  if (associated(c6f)) then
    deallocate(c6f)
    nullify(c6f)
  end if
  if (associated(cna)) then
    deallocate(cna)
    nullify(cna)
  end if
  if (associated(cnf)) then
    deallocate(cnf)
    nullify(cnf)
  end if
  if (associated(conadd)) then
    deallocate(conadd)
    nullify(conadd)
  end if
  if (associated(conco)) then
    deallocate(conco)
    nullify(conco)
  end if
  if (associated(mass)) then
    deallocate(mass)
    nullify(mass)
  end if
  if (associated(msdx)) then
    deallocate(msdx)
    nullify(msdx)
  end if
  if (associated(msdy)) then
    deallocate(msdy)
    nullify(msdy)
  end if
  if (associated(msdz)) then
    deallocate(msdz)
    nullify(msdz)
  end if
  if (associated(occua)) then
    deallocate(occua)
    nullify(occua)
  end if
  if (associated(occuf)) then
    deallocate(occuf)
    nullify(occuf)
  end if
  if (associated(oxa)) then
    deallocate(oxa)
    nullify(oxa)
  end if
  if (associated(oxf)) then
    deallocate(oxf)
    nullify(oxf)
  end if
  if (associated(qa)) then
    deallocate(qa)
    nullify(qa)
  end if
  if (associated(qf)) then
    deallocate(qf)
    nullify(qf)
  end if
  if (associated(rada)) then
    deallocate(rada)
    nullify(rada)
  end if
  if (associated(radf)) then
    deallocate(radf)
    nullify(radf)
  end if
  if (associated(rmass)) then
    deallocate(rmass)
    nullify(rmass)
  end if
  if (associated(temperaturestart)) then
    deallocate(temperaturestart)
    nullify(temperaturestart)
  end if
  if (associated(temperaturestep)) then
    deallocate(temperaturestep)
    nullify(temperaturestep)
  end if
  if (associated(xalat)) then
    deallocate(xalat)
    nullify(xalat)
  end if
  if (associated(yalat)) then
    deallocate(yalat)
    nullify(yalat)
  end if
  if (associated(zalat)) then
    deallocate(zalat)
    nullify(zalat)
  end if
  if (associated(xclat)) then
    deallocate(xclat)
    nullify(xclat)
  end if
  if (associated(yclat)) then
    deallocate(yclat)
    nullify(yclat)
  end if
  if (associated(zclat)) then
    deallocate(zclat)
    nullify(zclat)
  end if
  if (associated(xafrac)) then
    deallocate(xafrac)
    nullify(xafrac)
  end if
  if (associated(yafrac)) then
    deallocate(yafrac)
    nullify(yafrac)
  end if
  if (associated(zafrac)) then
    deallocate(zafrac)
    nullify(zafrac)
  end if
  if (associated(xfdmp)) then
    deallocate(xfdmp)
    nullify(xfdmp)
  end if
  if (associated(yfdmp)) then
    deallocate(yfdmp)
    nullify(yfdmp)
  end if
  if (associated(zfdmp)) then
    deallocate(zfdmp)
    nullify(zfdmp)
  end if
  if (associated(xfrac)) then
    deallocate(xfrac)
    nullify(xfrac)
  end if
  if (associated(yfrac)) then
    deallocate(yfrac)
    nullify(yfrac)
  end if
  if (associated(zfrac)) then
    deallocate(zfrac)
    nullify(zfrac)
  end if
  if (associated(xfracimage)) then
    deallocate(xfracimage)
    nullify(xfracimage)
  end if
  if (associated(yfracimage)) then
    deallocate(yfracimage)
    nullify(yfracimage)
  end if
  if (associated(zfracimage)) then
    deallocate(zfracimage)
    nullify(zfracimage)
  end if
  if (associated(xinitial)) then
    deallocate(xinitial)
    nullify(xinitial)
  end if
  if (associated(yinitial)) then
    deallocate(yinitial)
    nullify(yinitial)
  end if
  if (associated(zinitial)) then
    deallocate(zinitial)
    nullify(zinitial)
  end if
  if (associated(xstore)) then
    deallocate(xstore)
    nullify(xstore)
  end if
  if (associated(ystore)) then
    deallocate(ystore)
    nullify(ystore)
  end if
  if (associated(zstore)) then
    deallocate(zstore)
    nullify(zstore)
  end if
  if (associated(rstore)) then
    deallocate(rstore)
    nullify(rstore)
  end if

  !
  ! module defects
  !
  if (associated(nbondsdef)) then
    deallocate(nbondsdef)
    nullify(nbondsdef)
  end if
  if (associated(nbondeddef)) then
    deallocate(nbondeddef)
    nullify(nbondeddef)
  end if
  if (associated(nreldef)) then
    deallocate(nreldef)
    nullify(nreldef)
  end if
  if (associated(nreldefcfg)) then
    deallocate(nreldefcfg)
    nullify(nreldefcfg)
  end if
  if (associated(idoptindex)) then
    deallocate(idoptindex)
    nullify(idoptindex)
  end if
  if (associated(idopttype)) then
    deallocate(idopttype)
    nullify(idopttype)
  end if
  if (associated(idoptcfg)) then
    deallocate(idoptcfg)
    nullify(idoptcfg)
  end if
  if (associated(inddeffix)) then
    deallocate(inddeffix)
    nullify(inddeffix)
  end if
  if (associated(inddfix)) then
    deallocate(inddfix)
    nullify(inddfix)
  end if
  if (associated(natdefe)) then
    deallocate(natdefe)
    nullify(natdefe)
  end if
  if (associated(natdefecfg)) then
    deallocate(natdefecfg)
    nullify(natdefecfg)
  end if
  if (associated(natp)) then
    deallocate(natp)
    nullify(natp)
  end if
  if (associated(natdvacptrcfg)) then
    deallocate(natdvacptrcfg)
    nullify(natdvacptrcfg)
  end if
  if (associated(ntypdefe)) then
    deallocate(ntypdefe)
    nullify(ntypdefe)
  end if
  if (associated(ntypdefecfg)) then
    deallocate(ntypdefecfg)
    nullify(ntypdefecfg)
  end if
  if (associated(ntypep)) then
    deallocate(ntypep)
    nullify(ntypep)
  end if
  if (associated(ntypdvacptrcfg)) then
    deallocate(ntypdvacptrcfg)
    nullify(ntypdvacptrcfg)
  end if
  if (associated(ncdfixind)) then
    deallocate(ncdfixind)
    nullify(ncdfixind)
  end if
  if (associated(ncdfixtyp)) then
    deallocate(ncdfixtyp)
    nullify(ncdfixtyp)
  end if
  if (associated(ncdvarind)) then
    deallocate(ncdvarind)
    nullify(ncdvarind)
  end if
  if (associated(ncdvartyp)) then
    deallocate(ncdvartyp)
    nullify(ncdvartyp)
  end if
  if (associated(ndcentyp)) then
    deallocate(ndcentyp)
    nullify(ndcentyp)
  end if
  if (associated(ndefcfg)) then
    deallocate(ndefcfg)
    nullify(ndefcfg)
  end if
  if (associated(ndefind)) then
    deallocate(ndefind)
    nullify(ndefind)
  end if
  if (associated(ndefindcfg)) then
    deallocate(ndefindcfg)
    nullify(ndefindcfg)
  end if
  if (associated(ndefindp)) then
    deallocate(ndefindp)
    nullify(ndefindp)
  end if
  if (associated(ndefmol)) then
    deallocate(ndefmol)
    nullify(ndefmol)
  end if
  if (associated(ndefmolcfg)) then
    deallocate(ndefmolcfg)
    nullify(ndefmolcfg)
  end if
  if (associated(ndefmolp)) then
    deallocate(ndefmolp)
    nullify(ndefmolp)
  end if
  if (associated(ndefnat)) then
    deallocate(ndefnat)
    nullify(ndefnat)
  end if
  if (associated(ndeftp)) then
    deallocate(ndeftp)
    nullify(ndeftp)
  end if
  if (associated(ndeftyp)) then
    deallocate(ndeftyp)
    nullify(ndeftyp)
  end if
  if (associated(ndeqv)) then
    deallocate(ndeqv)
    nullify(ndeqv)
  end if
  if (associated(ndintptr)) then
    deallocate(ndintptr)
    nullify(ndintptr)
  end if
  if (associated(ndvacptr)) then
    deallocate(ndvacptr)
    nullify(ndvacptr)
  end if
  if (associated(ndintptrcfg)) then
    deallocate(ndintptrcfg)
    nullify(ndintptrcfg)
  end if
  if (associated(ndvacptrcfg)) then
    deallocate(ndvacptrcfg)
    nullify(ndvacptrcfg)
  end if
  if (associated(ndrel)) then
    deallocate(ndrel)
    nullify(ndrel)
  end if
  if (associated(ndrelop)) then
    deallocate(ndrelop)
    nullify(ndrelop)
  end if
  if (associated(ndsptr)) then
    deallocate(ndsptr)
    nullify(ndsptr)
  end if
  if (associated(npsite)) then
    deallocate(npsite)
    nullify(npsite)
  end if
  if (associated(nptrr1)) then
    deallocate(nptrr1)
    nullify(nptrr1)
  end if
  if (associated(nintecfg)) then
    deallocate(nintecfg)
    nullify(nintecfg)
  end if
  if (associated(nvacacfg)) then
    deallocate(nvacacfg)
    nullify(nvacacfg)
  end if
  if (associated(nreg1cfg)) then
    deallocate(nreg1cfg)
    nullify(nreg1cfg)
  end if
  if (associated(ldefbsmat)) then
    deallocate(ldefbsmat)
    nullify(ldefbsmat)
  end if
  if (associated(ldefbsmatcfg)) then
    deallocate(ldefbsmatcfg)
    nullify(ldefbsmatcfg)
  end if
  if (associated(lbrdvacptrcfg)) then
    deallocate(lbrdvacptrcfg)
    nullify(lbrdvacptrcfg)
  end if
  if (associated(ldeffix)) then
    deallocate(ldeffix)
    nullify(ldeffix)
  end if
  if (associated(ldeflin)) then
    deallocate(ldeflin)
    nullify(ldeflin)
  end if
  if (associated(ldfix)) then
    deallocate(ldfix)
    nullify(ldfix)
  end if
  if (associated(ldqmatom)) then
    deallocate(ldqmatom)
    nullify(ldqmatom)
  end if
  if (associated(ldqmatomcfg)) then
    deallocate(ldqmatomcfg)
    nullify(ldqmatomcfg)
  end if
  if (associated(lreldin)) then
    deallocate(lreldin)
    nullify(lreldin)
  end if
  if (associated(lr1created)) then
    deallocate(lr1created)
    nullify(lr1created)
  end if
  if (associated(lindintptr)) then
    deallocate(lindintptr)
    nullify(lindintptr)
  end if
  if (associated(lindvacptr)) then
    deallocate(lindvacptr)
    nullify(lindvacptr)
  end if
  if (associated(dconco)) then
    deallocate(dconco)
    nullify(dconco)
  end if
  if (associated(dscrho)) then
    deallocate(dscrho)
    nullify(dscrho)
  end if
  if (associated(occdefe)) then
    deallocate(occdefe)
    nullify(occdefe)
  end if
  if (associated(occdefecfg)) then
    deallocate(occdefecfg)
    nullify(occdefecfg)
  end if
  if (associated(occp)) then
    deallocate(occp)
    nullify(occp)
  end if
  if (associated(qdefe)) then
    deallocate(qdefe)
    nullify(qdefe)
  end if
  if (associated(qdefecfg)) then
    deallocate(qdefecfg)
    nullify(qdefecfg)
  end if
  if (associated(qp)) then
    deallocate(qp)
    nullify(qp)
  end if
  if (associated(radefe)) then
    deallocate(radefe)
    nullify(radefe)
  end if
  if (associated(radefecfg)) then
    deallocate(radefecfg)
    nullify(radefecfg)
  end if
  if (associated(reg1)) then
    deallocate(reg1)
    nullify(reg1)
  end if
  if (associated(reg1last)) then
    deallocate(reg1last)
    nullify(reg1last)
  end if
  if (associated(reg2)) then
    deallocate(reg2)
    nullify(reg2)
  end if
  if (associated(reg2a1)) then
    deallocate(reg2a1)
    nullify(reg2a1)
  end if
  if (associated(xdefe)) then
    deallocate(xdefe)
    nullify(xdefe)
  end if
  if (associated(ydefe)) then
    deallocate(ydefe)
    nullify(ydefe)
  end if
  if (associated(zdefe)) then
    deallocate(zdefe)
    nullify(zdefe)
  end if
  if (associated(xdefecfg)) then
    deallocate(xdefecfg)
    nullify(xdefecfg)
  end if
  if (associated(ydefecfg)) then
    deallocate(ydefecfg)
    nullify(ydefecfg)
  end if
  if (associated(zdefecfg)) then
    deallocate(zdefecfg)
    nullify(zdefecfg)
  end if
  if (associated(xperf)) then
    deallocate(xperf)
    nullify(xperf)
  end if
  if (associated(yperf)) then
    deallocate(yperf)
    nullify(yperf)
  end if
  if (associated(zperf)) then
    deallocate(zperf)
    nullify(zperf)
  end if
  if (associated(xdef)) then
    deallocate(xdef)
    nullify(xdef)
  end if
  if (associated(ydef)) then
    deallocate(ydef)
    nullify(ydef)
  end if
  if (associated(zdef)) then
    deallocate(zdef)
    nullify(zdef)
  end if
  if (associated(xdcent)) then
    deallocate(xdcent)
    nullify(xdcent)
  end if
  if (associated(ydcent)) then
    deallocate(ydcent)
    nullify(ydcent)
  end if
  if (associated(zdcent)) then
    deallocate(zdcent)
    nullify(zdcent)
  end if
  if (associated(xyzdvacptrcfg)) then
    deallocate(xyzdvacptrcfg)
    nullify(xyzdvacptrcfg)
  end if

  !
  ! module derivatives
  !
  if (associated(nd2cellptr)) then
    deallocate(nd2cellptr)
    nullify(nd2cellptr)
  end if
  if (associated(nqatoms)) then
    deallocate(nqatoms)
    nullify(nqatoms)
  end if
  if (associated(nqatomcell)) then
    deallocate(nqatomcell)
    nullify(nqatomcell)
  end if
  if (associated(nqatomptr)) then
    deallocate(nqatomptr)
    nullify(nqatomptr)
  end if
  if (associated(derv2dk)) then
    deallocate(derv2dk)
    nullify(derv2dk)
  end if
  if (associated(molQCdk)) then
    deallocate(molQCdk)
    nullify(molQCdk)
  end if
  if (associated(molQQdk)) then
    deallocate(molQQdk)
    nullify(molQQdk)
  end if
  if (associated(molQTdk)) then
    deallocate(molQTdk)
    nullify(molQTdk)
  end if
  if (associated(molTCdk)) then
    deallocate(molTCdk)
    nullify(molTCdk)
  end if
  if (associated(molTTdk)) then
    deallocate(molTTdk)
    nullify(molTTdk)
  end if
  if (associated(atomicstress)) then
    deallocate(atomicstress)
    nullify(atomicstress)
  end if
  if (associated(sumatomicstress)) then
    deallocate(sumatomicstress)
    nullify(sumatomicstress)
  end if
  if (associated(derv2)) then
    deallocate(derv2)
    nullify(derv2)
  end if
  if (associated(derv2d)) then
    deallocate(derv2d)
    nullify(derv2d)
  end if
  if (associated(dervi)) then
    deallocate(dervi)
    nullify(dervi)
  end if
  if (associated(derv3)) then
    deallocate(derv3)
    nullify(derv3)
  end if
  if (associated(diagblock)) then
    deallocate(diagblock)
    nullify(diagblock)
  end if
  if (associated(dqds)) then
    deallocate(dqds)
    nullify(dqds)
  end if
  if (associated(d2qds2)) then
    deallocate(d2qds2)
    nullify(d2qds2)
  end if
  if (associated(dqdxyz)) then
    deallocate(dqdxyz)
    nullify(dqdxyz)
  end if
  if (associated(d2qdxyz2)) then
    deallocate(d2qdxyz2)
    nullify(d2qdxyz2)
  end if
  if (associated(dedqc)) then
    deallocate(dedqc)
    nullify(dedqc)
  end if
  if (associated(d2edqc)) then
    deallocate(d2edqc)
    nullify(d2edqc)
  end if
  if (associated(d2edqdq)) then
    deallocate(d2edqdq)
    nullify(d2edqdq)
  end if
  if (associated(d2edq2)) then
    deallocate(d2edq2)
    nullify(d2edq2)
  end if
  if (associated(ds2g)) then
    deallocate(ds2g)
    nullify(ds2g)
  end if
  if (associated(ds2gs)) then
    deallocate(ds2gs)
    nullify(ds2gs)
  end if
  if (associated(d2s2gs)) then
    deallocate(d2s2gs)
    nullify(d2s2gs)
  end if
  if (associated(qatomxyz)) then
    deallocate(qatomxyz)
    nullify(qatomxyz)
  end if
  if (associated(d2qdxyzs)) then
    deallocate(d2qdxyzs)
    nullify(d2qdxyzs)
  end if
  if (associated(d2cell)) then
    deallocate(d2cell)
    nullify(d2cell)
  end if
  if (associated(molQdrv)) then
    deallocate(molQdrv)
    nullify(molQdrv)
  end if
  if (associated(molTdrv)) then
    deallocate(molTdrv)
    nullify(molTdrv)
  end if
  if (associated(molQCdrv)) then
    deallocate(molQCdrv)
    nullify(molQCdrv)
  end if
  if (associated(molQCdri)) then
    deallocate(molQCdri)
    nullify(molQCdri)
  end if
  if (associated(molQSdrv)) then
    deallocate(molQSdrv)
    nullify(molQSdrv)
  end if
  if (associated(molTCdrv)) then
    deallocate(molTCdrv)
    nullify(molTCdrv)
  end if
  if (associated(molTCdri)) then
    deallocate(molTCdri)
    nullify(molTCdri)
  end if
  if (associated(molTSdrv)) then
    deallocate(molTSdrv)
    nullify(molTSdrv)
  end if
  if (associated(molQQdrv)) then
    deallocate(molQQdrv)
    nullify(molQQdrv)
  end if
  if (associated(molQQdri)) then
    deallocate(molQQdri)
    nullify(molQQdri)
  end if
  if (associated(molQTdrv)) then
    deallocate(molQTdrv)
    nullify(molQTdrv)
  end if
  if (associated(molQTdri)) then
    deallocate(molQTdri)
    nullify(molQTdri)
  end if
  if (associated(molTTdrv)) then
    deallocate(molTTdrv)
    nullify(molTTdrv)
  end if
  if (associated(molTTdri)) then
    deallocate(molTTdri)
    nullify(molTTdri)
  end if
  if (associated(raderv)) then
    deallocate(raderv)
    nullify(raderv)
  end if
  if (associated(xdrv)) then
    deallocate(xdrv)
    nullify(xdrv)
  end if
  if (associated(ydrv)) then
    deallocate(ydrv)
    nullify(ydrv)
  end if
  if (associated(zdrv)) then
    deallocate(zdrv)
    nullify(zdrv)
  end if
  if (associated(xfdrv)) then
    deallocate(xfdrv)
    nullify(xfdrv)
  end if
  if (associated(yfdrv)) then
    deallocate(yfdrv)
    nullify(yfdrv)
  end if
  if (associated(zfdrv)) then
    deallocate(zfdrv)
    nullify(zfdrv)
  end if
  if (associated(xregdrv)) then
    deallocate(xregdrv)
    nullify(xregdrv)
  end if
  if (associated(yregdrv)) then
    deallocate(yregdrv)
    nullify(yregdrv)
  end if
  if (associated(zregdrv)) then
    deallocate(zregdrv)
    nullify(zregdrv)
  end if

  !
  ! module distances
  !
  if (associated(icosxs)) then
    deallocate(icosxs)
    nullify(icosxs)
  end if
  if (associated(icosys)) then
    deallocate(icosys)
    nullify(icosys)
  end if
  if (associated(icoszs)) then
    deallocate(icoszs)
    nullify(icoszs)
  end if
  if (associated(ndistance)) then
    deallocate(ndistance)
    nullify(ndistance)
  end if
  if (associated(ndistancecell)) then
    deallocate(ndistancecell)
    nullify(ndistancecell)
  end if
  if (associated(ndistanceij)) then
    deallocate(ndistanceij)
    nullify(ndistanceij)
  end if
  if (associated(ndistanceind)) then
    deallocate(ndistanceind)
    nullify(ndistanceind)
  end if
  if (associated(ndistancemolonly)) then
    deallocate(ndistancemolonly)
    nullify(ndistancemolonly)
  end if
  if (associated(ndistanceptr)) then
    deallocate(ndistanceptr)
    nullify(ndistanceptr)
  end if
  if (associated(ndistancereset)) then
    deallocate(ndistancereset)
    nullify(ndistancereset)
  end if
  if (associated(ndistbotype)) then
    deallocate(ndistbotype)
    nullify(ndistbotype)
  end if
  if (associated(ndistbotype2)) then
    deallocate(ndistbotype2)
    nullify(ndistbotype2)
  end if
  if (associated(distl1bond)) then
    deallocate(distl1bond)
    nullify(distl1bond)
  end if
  if (associated(distl2bond)) then
    deallocate(distl2bond)
    nullify(distl2bond)
  end if
  if (associated(distl3bond)) then
    deallocate(distl3bond)
    nullify(distl3bond)
  end if
  if (associated(distlptrmol)) then
    deallocate(distlptrmol)
    nullify(distlptrmol)
  end if
  if (associated(distlself)) then
    deallocate(distlself)
    nullify(distlself)
  end if
  if (associated(distance)) then
    deallocate(distance)
    nullify(distance)
  end if
  if (associated(distance2)) then
    deallocate(distance2)
    nullify(distance2)
  end if
  if (associated(distancexyz)) then
    deallocate(distancexyz)
    nullify(distancexyz)
  end if

  !
  ! module dispersion
  !
  if (associated(ndde)) then
    deallocate(ndde)
    nullify(ndde)
  end if
  if (associated(ndds)) then
    deallocate(ndds)
    nullify(ndds)
  end if
  if (associated(ndispcfg)) then
    deallocate(ndispcfg)
    nullify(ndispcfg)
  end if
  if (associated(ndstart)) then
    deallocate(ndstart)
    nullify(ndstart)
  end if
  if (associated(ndend)) then
    deallocate(ndend)
    nullify(ndend)
  end if
  if (associated(xdisp)) then
    deallocate(xdisp)
    nullify(xdisp)
  end if
  if (associated(ydisp)) then
    deallocate(ydisp)
    nullify(ydisp)
  end if
  if (associated(zdisp)) then
    deallocate(zdisp)
    nullify(zdisp)
  end if


  !
  ! module eam
  !
  if (associated(symboleamfnspec)) then
    deallocate(symboleamfnspec)
    nullify(symboleamfnspec)
  end if
  if (associated(symboleamspec)) then
    deallocate(symboleamspec)
    nullify(symboleamspec)
  end if
  if (associated(eamfnfile)) then
    deallocate(eamfnfile)
    nullify(eamfnfile)
  end if
  if (associated(eamdenfile)) then
    deallocate(eamdenfile)
    nullify(eamdenfile)
  end if
  if (associated(ndenfn)) then
    deallocate(ndenfn)
    nullify(ndenfn)
  end if
  if (associated(ndenfncomp)) then
    deallocate(ndenfncomp)
    nullify(ndenfncomp)
  end if
  if (associated(neamfnnumeric)) then
    deallocate(neamfnnumeric)
    nullify(neamfnnumeric)
  end if
  if (associated(neamnat)) then
    deallocate(neamnat)
    nullify(neamnat)
  end if
  if (associated(neamtyp)) then
    deallocate(neamtyp)
    nullify(neamtyp)
  end if
  if (associated(neamnat2)) then
    deallocate(neamnat2)
    nullify(neamnat2)
  end if
  if (associated(neamtyp2)) then
    deallocate(neamtyp2)
    nullify(neamtyp2)
  end if
  if (associated(neamfnnat)) then
    deallocate(neamfnnat)
    nullify(neamfnnat)
  end if
  if (associated(neamfntyp)) then
    deallocate(neamfntyp)
    nullify(neamfntyp)
  end if
  if (associated(neammeamorder)) then
    deallocate(neammeamorder)
    nullify(neammeamorder)
  end if
  if (associated(neamfnmeamorder)) then
    deallocate(neamfnmeamorder)
    nullify(neamfnmeamorder)
  end if
  if (associated(lmeamspec)) then
    deallocate(lmeamspec)
    nullify(lmeamspec)
  end if
  if (associated(lMEAMscreen)) then
    deallocate(lMEAMscreen)
    nullify(lMEAMscreen)
  end if
  if (associated(denpar)) then
    deallocate(denpar)
    nullify(denpar)
  end if
  if (associated(eamfnnumeric)) then
    deallocate(eamfnnumeric)
    nullify(eamfnnumeric)
  end if
  if (associated(eamfnnumeric1)) then
    deallocate(eamfnnumeric1)
    nullify(eamfnnumeric1)
  end if
  if (associated(eamfnnumeric2)) then
    deallocate(eamfnnumeric2)
    nullify(eamfnnumeric2)
  end if
  if (associated(eamfnnumeric3)) then
    deallocate(eamfnnumeric3)
    nullify(eamfnnumeric3)
  end if
  if (associated(eamfnnumeric4)) then
    deallocate(eamfnnumeric4)
    nullify(eamfnnumeric4)
  end if
  if (associated(eamfnnumeric5)) then
    deallocate(eamfnnumeric5)
    nullify(eamfnnumeric5)
  end if
  if (associated(eamfnnumeric6)) then
    deallocate(eamfnnumeric6)
    nullify(eamfnnumeric6)
  end if
  if (associated(eamfnnumeric7)) then
    deallocate(eamfnnumeric7)
    nullify(eamfnnumeric7)
  end if
  if (associated(eamfnnumeric8)) then
    deallocate(eamfnnumeric8)
    nullify(eamfnnumeric8)
  end if
  if (associated(eamfnnumericdrho)) then
    deallocate(eamfnnumericdrho)
    nullify(eamfnnumericdrho)
  end if
  if (associated(eamfnpar)) then
    deallocate(eamfnpar)
    nullify(eamfnpar)
  end if
  if (associated(eamalloy)) then
    deallocate(eamalloy)
    nullify(eamalloy)
  end if
  if (associated(eamfnmeamcoeff)) then
    deallocate(eamfnmeamcoeff)
    nullify(eamfnmeamcoeff)
  end if
  if (associated(eamtaperdrho)) then
    deallocate(eamtaperdrho)
    nullify(eamtaperdrho)
  end if
  if (associated(eamtaperrho)) then
    deallocate(eamtaperrho)
    nullify(eamtaperrho)
  end if
  if (associated(meam_Cmin)) then
    deallocate(meam_Cmin)
    nullify(meam_Cmin)
  end if
  if (associated(meam_Cmax)) then
    deallocate(meam_Cmax)
    nullify(meam_Cmax)
  end if
  ! use of 'public'
  ! deallocation of following commented varialbes : member of 'screening_atoms' within eam module requires further fix
  ! deallocation of following member variables are not touched; 06.2024 update
  !
  ! if (associated(sa_atom)) then
  !   deallocate(sa_atom)
  !   nullify(sa_atom)
  ! end if
  ! if (associated(sa_kxc)) then
  !   deallocate(sa_kxc)
  !   nullify(sa_kxc)
  ! end if
  ! if (associated(sa_rij)) then
  !   deallocate(sa_rij)
  !   nullify(sa_rij)
  ! end if
  ! if (associated(sa_rik)) then
  !   deallocate(sa_rik)
  !   nullify(sa_rik)
  ! end if
  ! if (associated(sa_rjk)) then
  !   deallocate(sa_rjk)
  !   nullify(sa_rjk)
  ! end if
  ! if (associated(sa_xik)) then
  !   deallocate(sa_xik)
  !   nullify(sa_xik)
  ! end if
  ! if (associated(sa_yik)) then
  !   deallocate(sa_yik)
  !   nullify(sa_yik)
  ! end if
  ! if (associated(sa_zik)) then
  !   deallocate(sa_zik)
  !   nullify(sa_zik)
  ! end if
  ! if (associated(sa_xjk)) then
  !   deallocate(sa_xjk)
  !   nullify(sa_xjk)
  ! end if
  ! if (associated(sa_yjk)) then
  !   deallocate(sa_yjk)
  !   nullify(sa_yjk)
  ! end if
  ! if (associated(sa_zjk)) then
  !   deallocate(sa_zjk)
  !   nullify(sa_zjk)
  ! end if
  ! if (associated(sa_Sikj)) then
  !   deallocate(sa_Sikj)
  !   nullify(sa_Sikj)
  ! end if
  ! if (associated(sa_dSikjdr)) then
  !   deallocate(sa_dSikjdr)
  !   nullify(sa_dSikjdr)
  ! end if
  ! if (associated(sa_drhototij)) then
  !   deallocate(sa_drhototij)
  !   nullify(sa_drhototij)
  ! end if
  ! if (associated(sa_drhototik)) then
  !   deallocate(sa_drhototik)
  !   nullify(sa_drhototik)
  ! end if
  ! if (associated(sa_drhototjk)) then
  !   deallocate(sa_drhototjk)
  !   nullify(sa_drhototjk)
  ! end if
  ! if (associated(sa_drhotots)) then
  !   deallocate(sa_drhotots)
  !   nullify(sa_drhotots)
  ! end if

  !
  ! module EDIPdata
  !
  if (associated(symbolEDIPspec)) then
    deallocate(symbolEDIPspec)
    nullify(symbolEDIPspec)
  end if
  if (associated(natEDIPspec)) then
    deallocate(natEDIPspec)
    nullify(natEDIPspec)
  end if
  if (associated(ntypEDIPspec)) then
    deallocate(ntypEDIPspec)
    nullify(ntypEDIPspec)
  end if
  if (associated(lEDIPpairOK)) then
    deallocate(lEDIPpairOK)
    nullify(lEDIPpairOK)
  end if
  if (associated(lEDIPtriadOK)) then
    deallocate(lEDIPtriadOK)
    nullify(lEDIPtriadOK)
  end if
  if (associated(lEDIPpi)) then
    deallocate(lEDIPpi)
    nullify(lEDIPpi)
  end if
  if (associated(lEDIP3mod)) then
    deallocate(lEDIP3mod)
    nullify(lEDIP3mod)
  end if
  if (associated(lEDIP3orig)) then
    deallocate(lEDIP3orig)
    nullify(lEDIP3orig)
  end if
  if (associated(EDIPrmax)) then
    deallocate(EDIPrmax)
    nullify(EDIPrmax)
  end if
  if (associated(EDIPrmaxpair)) then
    deallocate(EDIPrmaxpair)
    nullify(EDIPrmaxpair)
  end if
  if (associated(EDIPfhigh)) then
    deallocate(EDIPfhigh)
    nullify(EDIPfhigh)
  end if
  if (associated(EDIPflow)) then
    deallocate(EDIPflow)
    nullify(EDIPflow)
  end if
  if (associated(EDIPphigh)) then
    deallocate(EDIPphigh)
    nullify(EDIPphigh)
  end if
  if (associated(EDIPplow)) then
    deallocate(EDIPplow)
    nullify(EDIPplow)
  end if
  if (associated(EDIPalpha)) then
    deallocate(EDIPalpha)
    nullify(EDIPalpha)
  end if
  if (associated(EDIPZdih)) then
    deallocate(EDIPZdih)
    nullify(EDIPZdih)
  end if
  if (associated(EDIPZrep)) then
    deallocate(EDIPZrep)
    nullify(EDIPZrep)
  end if
  if (associated(EDIPc0)) then
    deallocate(EDIPc0)
    nullify(EDIPc0)
  end if
  if (associated(EDIP2epsilon)) then
    deallocate(EDIP2epsilon)
    nullify(EDIP2epsilon)
  end if
  if (associated(EDIP2a)) then
    deallocate(EDIP2a)
    nullify(EDIP2a)
  end if
  if (associated(EDIP2aprime)) then
    deallocate(EDIP2aprime)
    nullify(EDIP2aprime)
  end if
  if (associated(EDIP2B)) then
    deallocate(EDIP2B)
    nullify(EDIP2B)
  end if
  if (associated(EDIP2p)) then
    deallocate(EDIP2p)
    nullify(EDIP2p)
  end if
  if (associated(EDIP2beta)) then
    deallocate(EDIP2beta)
    nullify(EDIP2beta)
  end if
  if (associated(EDIP2sigma)) then
    deallocate(EDIP2sigma)
    nullify(EDIP2sigma)
  end if
  if (associated(EDIP3lambda0)) then
    deallocate(EDIP3lambda0)
    nullify(EDIP3lambda0)
  end if
  if (associated(EDIP3lambdap)) then
    deallocate(EDIP3lambdap)
    nullify(EDIP3lambdap)
  end if
  if (associated(EDIP3gamma0)) then
    deallocate(EDIP3gamma0)
    nullify(EDIP3gamma0)
  end if
  if (associated(EDIP3gammap)) then
    deallocate(EDIP3gammap)
    nullify(EDIP3gammap)
  end if
  if (associated(EDIP3q)) then
    deallocate(EDIP3q)
    nullify(EDIP3q)
  end if
  if (associated(EDIP3kq2)) then
    deallocate(EDIP3kq2)
    nullify(EDIP3kq2)
  end if
  if (associated(EDIP3Z0)) then
    deallocate(EDIP3Z0)
    nullify(EDIP3Z0)
  end if

  !
  ! module eembonds, eemdata
  !
  if (associated(neembonded)) then
    deallocate(neembonded)
    nullify(neembonded)
  end if
  if (associated(qbond)) then
    deallocate(qbond)
    nullify(qbond)
  end if ! end eembonds

  if (associated(nqrange)) then
    deallocate(nqrange)
    nullify(nqrange)
  end if
  if (associated(nqrangetype)) then
    deallocate(nqrangetype)
    nullify(nqrangetype)
  end if
  if (associated(nqrnow)) then
    deallocate(nqrnow)
    nullify(nqrnow)
  end if
  if (associated(qrangemax)) then
    deallocate(qrangemax)
    nullify(qrangemax)
  end if
  if (associated(qrangemin)) then
    deallocate(qrangemin)
    nullify(qrangemin)
  end if
  if (associated(chirange)) then
    deallocate(chirange)
    nullify(chirange)
  end if
  if (associated(murange)) then
    deallocate(murange)
    nullify(murange)
  end if
  if (associated(e0range)) then
    deallocate(e0range)
    nullify(e0range)
  end if
  if (associated(q0range)) then
    deallocate(q0range)
    nullify(q0range)
  end if
  if (associated(radrange)) then
    deallocate(radrange)
    nullify(radrange)
  end if
  if (associated(znucrange)) then
    deallocate(znucrange)
    nullify(znucrange)
  end if
  if (associated(zetarange)) then
    deallocate(zetarange)
    nullify(zetarange)
  end if ! end eemdata


  !
  ! module energies
  !
  if (associated(eregion2region)) then
    deallocate(eregion2region)
    nullify(eregion2region)
  end if
  if (associated(siteenergy)) then
    deallocate(siteenergy)
    nullify(siteenergy)
  end if


  !
  ! module feworkspace
  !
  if (associated(nptrfork)) then
    deallocate(nptrfork)
    nullify(nptrfork)
  end if
  if (associated(nptrforl)) then
    deallocate(nptrforl)
    nullify(nptrforl)
  end if
  if (associated(nptrmanyk)) then
    deallocate(nptrmanyk)
    nullify(nptrmanyk)
  end if
  if (associated(d33)) then
    deallocate(d33)
    nullify(d33)
  end if
  if (associated(d33r)) then
    deallocate(d33r)
    nullify(d33r)
  end if
  if (associated(d33i)) then
    deallocate(d33i)
    nullify(d33i)
  end if
  if (associated(d33s)) then
    deallocate(d33s)
    nullify(d33s)
  end if
  if (associated(d33rs)) then
    deallocate(d33rs)
    nullify(d33rs)
  end if
  if (associated(d33is)) then
    deallocate(d33is)
    nullify(d33is)
  end if
  if (associated(d34)) then
    deallocate(d34)
    nullify(d34)
  end if
  if (associated(d34r)) then
    deallocate(d34r)
    nullify(d34r)
  end if
  if (associated(d34i)) then
    deallocate(d34i)
    nullify(d34i)
  end if
  if (associated(d34s)) then
    deallocate(d34s)
    nullify(d34s)
  end if
  if (associated(d34rs)) then
    deallocate(d34rs)
    nullify(d34rs)
  end if
  if (associated(d34is)) then
    deallocate(d34is)
    nullify(d34is)
  end if

  !
  ! module field
  !
  if (associated(lfieldcfg)) then
    deallocate(lfieldcfg)
    nullify(lfieldcfg)
  end if
  if (associated(ntdfieldcfg)) then
    deallocate(ntdfieldcfg)
    nullify(ntdfieldcfg)
  end if
  if (associated(fieldcfg)) then
    deallocate(fieldcfg)
    nullify(fieldcfg)
  end if
  if (associated(fielddirectioncfg)) then
    deallocate(fielddirectioncfg)
    nullify(fielddirectioncfg)
  end if
  if (associated(td_fieldcfg)) then
    deallocate(td_fieldcfg)
    nullify(td_fieldcfg)
  end if
  if (associated(td_fielddirectioncfg)) then
    deallocate(td_fielddirectioncfg)
    nullify(td_fielddirectioncfg)
  end if

  !
  ! module gulp_gfnff
  !
  if (associated(gfnff_rcov)) then
    deallocate(gfnff_rcov)
    nullify(gfnff_rcov)
  end if
  if (associated(gfnff_rad)) then
    deallocate(gfnff_rad)
    nullify(gfnff_rad)
  end if
  if (associated(gfnff_rad_cn)) then
    deallocate(gfnff_rad_cn)
    nullify(gfnff_rad_cn)
  end if
  if (associated(gfnff_eeq_alp)) then
    deallocate(gfnff_eeq_alp)
    nullify(gfnff_eeq_alp)
  end if
  if (associated(gfnff_eeq_chi)) then
    deallocate(gfnff_eeq_chi)
    nullify(gfnff_eeq_chi)
  end if
  if (associated(gfnff_eeq_cnf)) then
    deallocate(gfnff_eeq_cnf)
    nullify(gfnff_eeq_cnf)
  end if
  if (associated(gfnff_eeq_gam)) then
    deallocate(gfnff_eeq_gam)
    nullify(gfnff_eeq_gam)
  end if
  if (associated(nfraglist)) then
    deallocate(nfraglist)
    nullify(nfraglist)
  end if
  if (associated(qfrag)) then
    deallocate(qfrag)
    nullify(qfrag)
  end if
  if (associated(gfnff_repulsion_a)) then
    deallocate(gfnff_repulsion_a)
    nullify(gfnff_repulsion_a)
  end if
  if (associated(gfnff_repulsion_z)) then
    deallocate(gfnff_repulsion_z)
    nullify(gfnff_repulsion_z)
  end if
  if (associated(gfnff_repulsion_p)) then
    deallocate(gfnff_repulsion_p)
    nullify(gfnff_repulsion_p)
  end if
  if (associated(n_gfnff_angleptr)) then
    deallocate(n_gfnff_angleptr)
    nullify(n_gfnff_angleptr)
  end if
  if (associated(par_gfnff_angle)) then
    deallocate(par_gfnff_angle)
    nullify(par_gfnff_angle)
  end if
  if (associated(n_gfnff_torsionptr)) then
    deallocate(n_gfnff_torsionptr)
    nullify(n_gfnff_torsionptr)
  end if
  if (associated(par_gfnff_torsion)) then
    deallocate(par_gfnff_torsion)
    nullify(par_gfnff_torsion)
  end if
  if (associated(nbond_hb_AH)) then
    deallocate(nbond_hb_AH)
    nullify(nbond_hb_AH)
  end if
  if (associated(nbond_hb_Bn)) then
    deallocate(nbond_hb_Bn)
    nullify(nbond_hb_Bn)
  end if
  if (associated(nbond_hb_B)) then
    deallocate(nbond_hb_B)
    nullify(nbond_hb_B)
  end if
  if (associated(n_gfnff_hb_ABptr)) then
    deallocate(n_gfnff_hb_ABptr)
    nullify(n_gfnff_hb_ABptr)
  end if
  if (associated(n_gfnff_xb_ABptr)) then
    deallocate(n_gfnff_xb_ABptr)
    nullify(n_gfnff_xb_ABptr)
  end if
  if (associated(n_gfnff_hb_Hptr)) then
    deallocate(n_gfnff_hb_Hptr)
    nullify(n_gfnff_hb_Hptr)
  end if
  if (associated(n_gfnff_hb_Hrptr)) then
    deallocate(n_gfnff_hb_Hrptr)
    nullify(n_gfnff_hb_Hrptr)
  end if
  if (associated(gfnff_hb_ABq)) then
    deallocate(gfnff_hb_ABq)
    nullify(gfnff_hb_ABq)
  end if
  if (associated(gfnff_hb_acid)) then
    deallocate(gfnff_hb_acid)
    nullify(gfnff_hb_acid)
  end if
  if (associated(gfnff_hb_base)) then
    deallocate(gfnff_hb_base)
    nullify(gfnff_hb_base)
  end if
  if (associated(gfnff_xb_ABq)) then
    deallocate(gfnff_xb_ABq)
    nullify(gfnff_xb_ABq)
  end if
  if (associated(gfnff_xb_scale)) then
    deallocate(gfnff_xb_scale)
    nullify(gfnff_xb_scale)
  end if

  !
  ! module fitting
  !
  if (associated(nfatyp)) then
    deallocate(nfatyp)
    nullify(nfatyp)
  end if
  if (associated(nfcfix)) then
    deallocate(nfcfix)
    nullify(nfcfix)
  end if
  if (associated(nfcotyp)) then
    deallocate(nfcotyp)
    nullify(nfcotyp)
  end if
  if (associated(nfcvar)) then
    deallocate(nfcvar)
    nullify(nfcvar)
  end if
  if (associated(nfcfg)) then
    deallocate(nfcfg)
    nullify(nfcfg)
  end if
  if (associated(nfpot)) then
    deallocate(nfpot)
    nullify(nfpot)
  end if
  if (associated(nfpot2)) then
    deallocate(nfpot2)
    nullify(nfpot2)
  end if
  if (associated(nfpot3)) then
    deallocate(nfpot3)
    nullify(nfpot3)
  end if
  if (associated(nftyp)) then
    deallocate(nftyp)
    nullify(nftyp)
  end if
  if (associated(nfvar)) then
    deallocate(nfvar)
    nullify(nfvar)
  end if
  if (associated(nfvar2)) then
    deallocate(nfvar2)
    nullify(nfvar2)
  end if
  if (associated(nfvar3)) then
    deallocate(nfvar3)
    nullify(nfvar3)
  end if
  if (associated(nfitptr)) then
    deallocate(nfitptr)
    nullify(nfitptr)
  end if
  if (associated(lsumcoreshell)) then
    deallocate(lsumcoreshell)
    nullify(lsumcoreshell)
  end if
  if (associated(fconadd)) then
    deallocate(fconadd)
    nullify(fconadd)
  end if
  if (associated(fconco)) then
    deallocate(fconco)
    nullify(fconco)
  end if
  if (associated(fconpower)) then
    deallocate(fconpower)
    nullify(fconpower)
  end if
  if (associated(fres)) then
    deallocate(fres)
    nullify(fres)
  end if
  if (associated(scale)) then
    deallocate(scale)
    nullify(scale)
  end if

  !
  ! module four
  !
  if (associated(symbol4)) then
    deallocate(symbol4)
    nullify(symbol4)
  end if
  if (associated(icell41)) then
    deallocate(icell41)
    nullify(icell41)
  end if
  if (associated(icell42)) then
    deallocate(icell42)
    nullify(icell42)
  end if
  if (associated(icell43)) then
    deallocate(icell43)
    nullify(icell43)
  end if
  if (associated(ilind)) then
    deallocate(ilind)
    nullify(ilind)
  end if
  if (associated(ilnum)) then
    deallocate(ilnum)
    nullify(ilnum)
  end if
  if (associated(iltor)) then
    deallocate(iltor)
    nullify(iltor)
  end if
  if (associated(ilftor)) then
    deallocate(ilftor)
    nullify(ilftor)
  end if
  if (associated(ilxtor)) then
    deallocate(ilxtor)
    nullify(ilxtor)
  end if
  if (associated(jkind)) then
    deallocate(jkind)
    nullify(jkind)
  end if
  if (associated(mmfexc)) then
    deallocate(mmfexc)
    nullify(mmfexc)
  end if
  if (associated(n4botype)) then
    deallocate(n4botype)
    nullify(n4botype)
  end if
  if (associated(nctor)) then
    deallocate(nctor)
    nullify(nctor)
  end if
  if (associated(neqiltor)) then
    deallocate(neqiltor)
    nullify(neqiltor)
  end if
  if (associated(nforptr)) then
    deallocate(nforptr)
    nullify(nforptr)
  end if
  if (associated(nforty)) then
    deallocate(nforty)
    nullify(nforty)
  end if
  if (associated(nfptyp1)) then
    deallocate(nfptyp1)
    nullify(nfptyp1)
  end if
  if (associated(nfptyp2)) then
    deallocate(nfptyp2)
    nullify(nfptyp2)
  end if
  if (associated(nfptyp3)) then
    deallocate(nfptyp3)
    nullify(nfptyp3)
  end if
  if (associated(nfptyp4)) then
    deallocate(nfptyp4)
    nullify(nfptyp4)
  end if
  if (associated(nfspec1)) then
    deallocate(nfspec1)
    nullify(nfspec1)
  end if
  if (associated(nfspec2)) then
    deallocate(nfspec2)
    nullify(nfspec2)
  end if
  if (associated(nfspec3)) then
    deallocate(nfspec3)
    nullify(nfspec3)
  end if
  if (associated(nfspec4)) then
    deallocate(nfspec4)
    nullify(nfspec4)
  end if
  if (associated(npfor)) then
    deallocate(npfor)
    nullify(npfor)
  end if
  if (associated(nduptor)) then
    deallocate(nduptor)
    nullify(nduptor)
  end if
  if (associated(nfortor)) then
    deallocate(nfortor)
    nullify(nfortor)
  end if
  if (associated(nkeeptor)) then
    deallocate(nkeeptor)
    nullify(nkeeptor)
  end if
  if (associated(nwildduptor)) then
    deallocate(nwildduptor)
    nullify(nwildduptor)
  end if
  if (associated(lfdreiding)) then
    deallocate(lfdreiding)
    nullify(lfdreiding)
  end if
  if (associated(lfintra)) then
    deallocate(lfintra)
    nullify(lfintra)
  end if
  if (associated(lfinter)) then
    deallocate(lfinter)
    nullify(lfinter)
  end if
  if (associated(lgenerated4)) then
    deallocate(lgenerated4)
    nullify(lgenerated4)
  end if
  if (associated(lkeeptor)) then
    deallocate(lkeeptor)
    nullify(lkeeptor)
  end if
  if (associated(lonly3oop)) then
    deallocate(lonly3oop)
    nullify(lonly3oop)
  end if
  if (associated(lopiltor)) then
    deallocate(lopiltor)
    nullify(lopiltor)
  end if
  if (associated(loutofplane)) then
    deallocate(loutofplane)
    nullify(loutofplane)
  end if
  if (associated(liltorswitch)) then
    deallocate(liltorswitch)
    nullify(liltorswitch)
  end if
  if (associated(ljktorswitch)) then
    deallocate(ljktorswitch)
    nullify(ljktorswitch)
  end if
  if (associated(lsurfiltor)) then
    deallocate(lsurfiltor)
    nullify(lsurfiltor)
  end if
  if (associated(ltib4body)) then
    deallocate(ltib4body)
    nullify(ltib4body)
  end if
  if (associated(ltif4body)) then
    deallocate(ltif4body)
    nullify(ltif4body)
  end if
  if (associated(fork)) then
    deallocate(fork)
    nullify(fork)
  end if
  if (associated(for1)) then
    deallocate(for1)
    nullify(for1)
  end if
  if (associated(for2)) then
    deallocate(for2)
    nullify(for2)
  end if
  if (associated(for3)) then
    deallocate(for3)
    nullify(for3)
  end if
  if (associated(for4)) then
    deallocate(for4)
    nullify(for4)
  end if
  if (associated(for1min)) then
    deallocate(for1min)
    nullify(for1min)
  end if
  if (associated(for2min)) then
    deallocate(for2min)
    nullify(for2min)
  end if
  if (associated(for3min)) then
    deallocate(for3min)
    nullify(for3min)
  end if
  if (associated(for4min)) then
    deallocate(for4min)
    nullify(for4min)
  end if
  if (associated(forpoly)) then
    deallocate(forpoly)
    nullify(forpoly)
  end if
  if (associated(oiltor)) then
    deallocate(oiltor)
    nullify(oiltor)
  end if
  if (associated(riltor)) then
    deallocate(riltor)
    nullify(riltor)
  end if
  if (associated(xcomtor)) then
    deallocate(xcomtor)
    nullify(xcomtor)
  end if
  if (associated(ycomtor)) then
    deallocate(ycomtor)
    nullify(ycomtor)
  end if
  if (associated(zcomtor)) then
    deallocate(zcomtor)
    nullify(zcomtor)
  end if
  if (associated(xiltor)) then
    deallocate(xiltor)
    nullify(xiltor)
  end if
  if (associated(yiltor)) then
    deallocate(yiltor)
    nullify(yiltor)
  end if
  if (associated(ziltor)) then
    deallocate(ziltor)
    nullify(ziltor)
  end if

  !
  ! module frequencies
  !
  if (associated(fmass)) then
    deallocate(fmass)
    nullify(fmass)
  end if
  if (associated(freq)) then
    deallocate(freq)
    nullify(freq)
  end if
  if (associated(rfmass)) then
    deallocate(rfmass)
    nullify(rfmass)
  end if
  if (associated(groupvelocity)) then
    deallocate(groupvelocity)
    nullify(groupvelocity)
  end if
  if (associated(grueneisen)) then
    deallocate(grueneisen)
    nullify(grueneisen)
  end if
  if (associated(IRintensity)) then
    deallocate(IRintensity)
    nullify(IRintensity)
  end if
  if (associated(eigv)) then
    deallocate(eigv)
    nullify(eigv)
  end if
  if (associated(aphase)) then
    deallocate(aphase)
    nullify(aphase)
  end if
  if (associated(fphase)) then
    deallocate(fphase)
    nullify(fphase)
  end if

  !
  ! module gaconf
  !
  if (associated(ithbest)) then
    deallocate(ithbest)
    nullify(ithbest)
  end if
  if (associated(xconf)) then
    deallocate(xconf)
    nullify(xconf)
  end if
  if (associated(xbest)) then
    deallocate(xbest)
    nullify(xbest)
  end if
  if (associated(fconf)) then
    deallocate(fconf)
    nullify(fconf)
  end if


  !
  ! module general
  !
  if (associated(titleword)) then
    deallocate(titleword)
    nullify(titleword)
  end if


  !
  ! module genetic
  !
  if (associated(ndiscret)) then
    deallocate(ndiscret)
    nullify(ndiscret)
  end if
  if (associated(xmax)) then
    deallocate(xmax)
    nullify(xmax)
  end if
  if (associated(xmaxcfg)) then
    deallocate(xmaxcfg)
    nullify(xmaxcfg)
  end if
  if (associated(xmin)) then
    deallocate(xmin)
    nullify(xmin)
  end if
  if (associated(xmincfg)) then
    deallocate(xmincfg)
    nullify(xmincfg)
  end if


  !
  ! module gulpinput
  !
  if (associated(floatwords)) then
    deallocate(floatwords)
    nullify(floatwords)
  end if
  if (associated(words)) then
    deallocate(words)
    nullify(words)
  end if
  if (associated(nlorder)) then
    deallocate(nlorder)
    nullify(nlorder)
  end if
  if (associated(floats)) then
    deallocate(floats)
    nullify(floats)
  end if

  !
  ! module ksample
  !
  if (associated(nkptcfg)) then
    deallocate(nkptcfg)
    nullify(nkptcfg)
  end if
  if (associated(norigkpt)) then
    deallocate(norigkpt)
    nullify(norigkpt)
  end if
  if (associated(nks)) then
    deallocate(nks)
    nullify(nks)
  end if
  if (associated(nksala)) then
    deallocate(nksala)
    nullify(nksala)
  end if
  if (associated(lkptdispersion)) then
    deallocate(lkptdispersion)
    nullify(lkptdispersion)
  end if
  if (associated(lksorigin)) then
    deallocate(lksorigin)
    nullify(lksorigin)
  end if
  if (associated(wkpt)) then
    deallocate(wkpt)
    nullify(wkpt)
  end if
  if (associated(xkpt)) then
    deallocate(xkpt)
    nullify(xkpt)
  end if
  if (associated(ykpt)) then
    deallocate(ykpt)
    nullify(ykpt)
  end if
  if (associated(zkpt)) then
    deallocate(zkpt)
    nullify(zkpt)
  end if

  !
  ! module ksample_scatter
  !
  if (associated(wskpt)) then
    deallocate(wskpt)
    nullify(wskpt)
  end if
  if (associated(xskpt)) then
    deallocate(xskpt)
    nullify(xskpt)
  end if
  if (associated(yskpt)) then
    deallocate(yskpt)
    nullify(yskpt)
  end if
  if (associated(zskpt)) then
    deallocate(zskpt)
    nullify(zskpt)
  end if

  !
  ! module kspace
  !
  if (associated(indk)) then
    deallocate(indk)
    nullify(indk)
  end if
  if (associated(argc)) then
    deallocate(argc)
    nullify(argc)
  end if
  if (associated(csin)) then
    deallocate(csin)
    nullify(csin)
  end if
  if (associated(kmod)) then
    deallocate(kmod)
    nullify(kmod)
  end if
  if (associated(ktrm)) then
    deallocate(ktrm)
    nullify(ktrm)
  end if
  if (associated(ktrms)) then
    deallocate(ktrms)
    nullify(ktrms)
  end if
  if (associated(ktrms2)) then
    deallocate(ktrms2)
    nullify(ktrms2)
  end if
  if (associated(sine)) then
    deallocate(sine)
    nullify(sine)
  end if
  if (associated(xrk)) then
    deallocate(xrk)
    nullify(xrk)
  end if
  if (associated(yrk)) then
    deallocate(yrk)
    nullify(yrk)
  end if
  if (associated(zrk)) then
    deallocate(zrk)
    nullify(zrk)
  end if
  if (associated(xrk0)) then
    deallocate(xrk0)
    nullify(xrk0)
  end if
  if (associated(yrk0)) then
    deallocate(yrk0)
    nullify(yrk0)
  end if
  if (associated(zrk0)) then
    deallocate(zrk0)
    nullify(zrk0)
  end if
  if (associated(dgds)) then
    deallocate(dgds)
    nullify(dgds)
  end if
  if (associated(dg2ds)) then
    deallocate(dg2ds)
    nullify(dg2ds)
  end if
  if (associated(d2gds2)) then
    deallocate(d2gds2)
    nullify(d2gds2)
  end if
  if (associated(d2g2ds2)) then
    deallocate(d2g2ds2)
    nullify(d2g2ds2)
  end if
  if (associated(d2g2dx2)) then
    deallocate(d2g2dx2)
    nullify(d2g2dx2)
  end if


  !
  ! module library
  !
  if (associated(libname)) then
    deallocate(libname)
    nullify(libname)
  end if
  if (associated(libspec)) then
    deallocate(libspec)
    nullify(libspec)
  end if
  if (associated(lib2ndspec)) then
    deallocate(lib2ndspec)
    nullify(lib2ndspec)
  end if
  if (associated(llib2ndspec)) then
    deallocate(llib2ndspec)
    nullify(llib2ndspec)
  end if

  !
  ! module montecarlo
  !
  if (associated(ngcmcnat)) then
    deallocate(ngcmcnat)
    nullify(ngcmcnat)
  end if
  if (associated(ngcmctype)) then
    deallocate(ngcmctype)
    nullify(ngcmctype)
  end if
  if (associated(ngcmcmolat)) then
    deallocate(ngcmcmolat)
    nullify(ngcmcmolat)
  end if
  if (associated(ngcmcmolnat)) then
    deallocate(ngcmcmolnat)
    nullify(ngcmcmolnat)
  end if
  if (associated(ngcmcmoltype)) then
    deallocate(ngcmcmoltype)
    nullify(ngcmcmoltype)
  end if
  if (associated(nptrdestroyable)) then
    deallocate(nptrdestroyable)
    nullify(nptrdestroyable)
  end if
  if (associated(nptrmoveable)) then
    deallocate(nptrmoveable)
    nullify(nptrmoveable)
  end if
  if (associated(nptrrotateable)) then
    deallocate(nptrrotateable)
    nullify(nptrrotateable)
  end if
  if (associated(nptrstrainable)) then
    deallocate(nptrstrainable)
    nullify(nptrstrainable)
  end if
  if (associated(nptrswapable)) then
    deallocate(nptrswapable)
    nullify(nptrswapable)
  end if
  if (associated(nptrtranable)) then
    deallocate(nptrtranable)
    nullify(nptrtranable)
  end if
  if (associated(nptrtrialatom)) then
    deallocate(nptrtrialatom)
    nullify(nptrtrialatom)
  end if
  if (associated(nmcswapnat)) then
    deallocate(nmcswapnat)
    nullify(nmcswapnat)
  end if
  if (associated(nmcswappair)) then
    deallocate(nmcswappair)
    nullify(nmcswappair)
  end if
  if (associated(nmcswapspec)) then
    deallocate(nmcswapspec)
    nullify(nmcswapspec)
  end if
  if (associated(nmcswaptype)) then
    deallocate(nmcswaptype)
    nullify(nmcswaptype)
  end if
  if (associated(nmctrannat)) then
    deallocate(nmctrannat)
    nullify(nmctrannat)
  end if
  if (associated(nmctranspec)) then
    deallocate(nmctranspec)
    nullify(nmctranspec)
  end if
  if (associated(nmctrantype)) then
    deallocate(nmctrantype)
    nullify(nmctrantype)
  end if
  if (associated(nswapable)) then
    deallocate(nswapable)
    nullify(nswapable)
  end if
  if (associated(ntranable)) then
    deallocate(ntranable)
    nullify(ntranable)
  end if
  if (associated(ltrialatom)) then
    deallocate(ltrialatom)
    nullify(ltrialatom)
  end if
  if (associated(lmcswapany)) then
    deallocate(lmcswapany)
    nullify(lmcswapany)
  end if
  if (associated(pswap)) then
    deallocate(pswap)
    nullify(pswap)
  end if
  if (associated(ptran)) then
    deallocate(ptran)
    nullify(ptran)
  end if
  if (associated(xgcmcmol)) then
    deallocate(xgcmcmol)
    nullify(xgcmcmol)
  end if
  if (associated(ygcmcmol)) then
    deallocate(ygcmcmol)
    nullify(ygcmcmol)
  end if
  if (associated(zgcmcmol)) then
    deallocate(zgcmcmol)
    nullify(zgcmcmol)
  end if


  !
  ! module moldyn
  !
  if (associated(nensemble)) then
    deallocate(nensemble)
    nullify(nensemble)
  end if
  if (associated(nmdconstrainatom)) then
    deallocate(nmdconstrainatom)
    nullify(nmdconstrainatom)
  end if
  if (associated(nmdeq)) then
    deallocate(nmdeq)
    nullify(nmdeq)
  end if
  if (associated(nmdprod)) then
    deallocate(nmdprod)
    nullify(nmdprod)
  end if
  if (associated(nmdsamp)) then
    deallocate(nmdsamp)
    nullify(nmdsamp)
  end if
  if (associated(nmdvelmode)) then
    deallocate(nmdvelmode)
    nullify(nmdvelmode)
  end if
  if (associated(nmdvelmodp)) then
    deallocate(nmdvelmodp)
    nullify(nmdvelmodp)
  end if
  if (associated(nmdwrite)) then
    deallocate(nmdwrite)
    nullify(nmdwrite)
  end if
  if (associated(labsco)) then
    deallocate(labsco)
    nullify(labsco)
  end if
  if (associated(lfix)) then
    deallocate(lfix)
    nullify(lfix)
  end if
  if (associated(lmdconstrain)) then
    deallocate(lmdconstrain)
    nullify(lmdconstrain)
  end if
  if (associated(nmdconstraindist)) then
    deallocate(nmdconstraindist)
    nullify(nmdconstraindist)
  end if
  if (associated(qpres)) then
    deallocate(qpres)
    nullify(qpres)
  end if
  if (associated(qtemp)) then
    deallocate(qtemp)
    nullify(qtemp)
  end if
  if (associated(tmdforcestart)) then
    deallocate(tmdforcestart)
    nullify(tmdforcestart)
  end if
  if (associated(tmdforcestop)) then
    deallocate(tmdforcestop)
    nullify(tmdforcestop)
  end if
  if (associated(tmdfieldstart)) then
    deallocate(tmdfieldstart)
    nullify(tmdfieldstart)
  end if
  if (associated(tmdfieldstop)) then
    deallocate(tmdfieldstop)
    nullify(tmdfieldstop)
  end if
  if (associated(tmdeq)) then
    deallocate(tmdeq)
    nullify(tmdeq)
  end if
  if (associated(tmdprod)) then
    deallocate(tmdprod)
    nullify(tmdprod)
  end if
  if (associated(tmdsamp)) then
    deallocate(tmdsamp)
    nullify(tmdsamp)
  end if
  if (associated(tmdscale)) then
    deallocate(tmdscale)
    nullify(tmdscale)
  end if
  if (associated(tmdscint)) then
    deallocate(tmdscint)
    nullify(tmdscint)
  end if
  if (associated(tmdwrite)) then
    deallocate(tmdwrite)
    nullify(tmdwrite)
  end if
  if (associated(tstep)) then
    deallocate(tstep)
    nullify(tstep)
  end if
  if (associated(xabsco)) then
    deallocate(xabsco)
    nullify(xabsco)
  end if
  if (associated(yabsco)) then
    deallocate(yabsco)
    nullify(yabsco)
  end if
  if (associated(zabsco)) then
    deallocate(zabsco)
    nullify(zabsco)
  end if

  !
  ! module molecule
  !
  if (associated(moldim)) then
    deallocate(moldim)
    nullify(moldim)
  end if
  if (associated(moldimi)) then
    deallocate(moldimi)
    nullify(moldimi)
  end if
  if (associated(molgcmc)) then
    deallocate(molgcmc)
    nullify(molgcmc)
  end if
  if (associated(ixshift)) then
    deallocate(ixshift)
    nullify(ixshift)
  end if
  if (associated(iyshift)) then
    deallocate(iyshift)
    nullify(iyshift)
  end if
  if (associated(izshift)) then
    deallocate(izshift)
    nullify(izshift)
  end if
  if (associated(natmol)) then
    deallocate(natmol)
    nullify(natmol)
  end if
  if (associated(natinmol)) then
    deallocate(natinmol)
    nullify(natinmol)
  end if
  if (associated(n1connect)) then
    deallocate(n1connect)
    nullify(n1connect)
  end if
  if (associated(n2connect)) then
    deallocate(n2connect)
    nullify(n2connect)
  end if
  if (associated(nconnectcfg)) then
    deallocate(nconnectcfg)
    nullify(nconnectcfg)
  end if
  if (associated(nconnectind)) then
    deallocate(nconnectind)
    nullify(nconnectind)
  end if
  if (associated(nconnecttype)) then
    deallocate(nconnecttype)
    nullify(nconnecttype)
  end if
  if (associated(nmola2f)) then
    deallocate(nmola2f)
    nullify(nmola2f)
  end if
  if (associated(nmolf2a)) then
    deallocate(nmolf2a)
    nullify(nmolf2a)
  end if
  if (associated(nmolasymno)) then
    deallocate(nmolasymno)
    nullify(nmolasymno)
  end if
  if (associated(nmolasymptr)) then
    deallocate(nmolasymptr)
    nullify(nmolasymptr)
  end if
  if (associated(nmolasymeqvptr)) then
    deallocate(nmolasymeqvptr)
    nullify(nmolasymeqvptr)
  end if
  if (associated(nmolatom)) then
    deallocate(nmolatom)
    nullify(nmolatom)
  end if
  if (associated(nmolatomcfg)) then
    deallocate(nmolatomcfg)
    nullify(nmolatomcfg)
  end if
  if (associated(nmolatomtotcfg)) then
    deallocate(nmolatomtotcfg)
    nullify(nmolatomtotcfg)
  end if
  if (associated(nmolconnect)) then
    deallocate(nmolconnect)
    nullify(nmolconnect)
  end if
  if (associated(nmolconnectptr)) then
    deallocate(nmolconnectptr)
    nullify(nmolconnectptr)
  end if
  if (associated(nmolcore)) then
    deallocate(nmolcore)
    nullify(nmolcore)
  end if
  if (associated(nmolcorecfg)) then
    deallocate(nmolcorecfg)
    nullify(nmolcorecfg)
  end if
  if (associated(nmolcfg)) then
    deallocate(nmolcfg)
    nullify(nmolcfg)
  end if
  if (associated(nmoleqv)) then
    deallocate(nmoleqv)
    nullify(nmoleqv)
  end if
  if (associated(nmolind)) then
    deallocate(nmolind)
    nullify(nmolind)
  end if
  if (associated(nmollist)) then
    deallocate(nmollist)
    nullify(nmollist)
  end if
  if (associated(nmollistcfg)) then
    deallocate(nmollistcfg)
    nullify(nmollistcfg)
  end if
  if (associated(nmolptr)) then
    deallocate(nmolptr)
    nullify(nmolptr)
  end if
  if (associated(nobond)) then
    deallocate(nobond)
    nullify(nobond)
  end if
  if (associated(nobotyp)) then
    deallocate(nobotyp)
    nullify(nobotyp)
  end if
  if (associated(natbondtype)) then
    deallocate(natbondtype)
    nullify(natbondtype)
  end if
  if (associated(ntypbondtype)) then
    deallocate(ntypbondtype)
    nullify(ntypbondtype)
  end if
  if (associated(nbondtypeptr)) then
    deallocate(nbondtypeptr)
    nullify(nbondtypeptr)
  end if
  if (associated(lgcmcmol)) then
    deallocate(lgcmcmol)
    nullify(lgcmcmol)
  end if
  if (associated(molaxes)) then
    deallocate(molaxes)
    nullify(molaxes)
  end if
  if (associated(molcomcfg)) then
    deallocate(molcomcfg)
    nullify(molcomcfg)
  end if
  if (associated(molQcfg)) then
    deallocate(molQcfg)
    nullify(molQcfg)
  end if
  if (associated(molQxyzcfg)) then
    deallocate(molQxyzcfg)
    nullify(molQxyzcfg)
  end if
  if (associated(molcom)) then
    deallocate(molcom)
    nullify(molcom)
  end if
  if (associated(molcoma)) then
    deallocate(molcoma)
    nullify(molcoma)
  end if
  if (associated(molI)) then
    deallocate(molI)
    nullify(molI)
  end if
  if (associated(molQ)) then
    deallocate(molQ)
    nullify(molQ)
  end if
  if (associated(molQa)) then
    deallocate(molQa)
    nullify(molQa)
  end if
  if (associated(molQeig)) then
    deallocate(molQeig)
    nullify(molQeig)
  end if
  if (associated(molQsym)) then
    deallocate(molQsym)
    nullify(molQsym)
  end if
  if (associated(molxyz)) then
    deallocate(molxyz)
    nullify(molxyz)
  end if
  if (associated(molQxyz)) then
    deallocate(molQxyz)
    nullify(molQxyz)
  end if
  if (associated(molQd1)) then
    deallocate(molQd1)
    nullify(molQd1)
  end if
  if (associated(xfsave)) then
    deallocate(xfsave)
    nullify(xfsave)
  end if
  if (associated(yfsave)) then
    deallocate(yfsave)
    nullify(yfsave)
  end if
  if (associated(zfsave)) then
    deallocate(zfsave)
    nullify(zfsave)
  end if

  !
  ! module g_neb
  !
  if (associated(nnebreplica)) then
    deallocate(nnebreplica)
    nullify(nnebreplica)
  end if
  if (associated(nnebreplicano)) then
    deallocate(nnebreplicano)
    nullify(nnebreplicano)
  end if
  if (associated(nebreplicacfgptr)) then
    deallocate(nebreplicacfgptr)
    nullify(nebreplicacfgptr)
  end if
  if (associated(lnebvaryspring)) then
    deallocate(lnebvaryspring)
    nullify(lnebvaryspring)
  end if
  if (associated(nebspring)) then
    deallocate(nebspring)
    nullify(nebspring)
  end if
  if (associated(nebspringmin)) then
    deallocate(nebspringmin)
    nullify(nebspringmin)
  end if
  if (associated(nebfinalcell)) then
    deallocate(nebfinalcell)
    nullify(nebfinalcell)
  end if
  if (associated(nebfinalradius)) then
    deallocate(nebfinalradius)
    nullify(nebfinalradius)
  end if
  if (associated(nebfinalxyz)) then
    deallocate(nebfinalxyz)
    nullify(nebfinalxyz)
  end if
  if (associated(nebreplicacell)) then
    deallocate(nebreplicacell)
    nullify(nebreplicacell)
  end if
  if (associated(nebreplicaradius)) then
    deallocate(nebreplicaradius)
    nullify(nebreplicaradius)
  end if
  if (associated(nebreplicaxyz)) then
    deallocate(nebreplicaxyz)
    nullify(nebreplicaxyz)
  end if

  !
  ! module observables
  !
  if (associated(nfgracfg)) then
    deallocate(nfgracfg)
    nullify(nfgracfg)
  end if
  if (associated(nfgrat)) then
    deallocate(nfgrat)
    nullify(nfgrat)
  end if
  if (associated(nfstraincfg)) then
    deallocate(nfstraincfg)
    nullify(nfstraincfg)
  end if
  if (associated(nfstraint)) then
    deallocate(nfstraint)
    nullify(nfstraint)
  end if
  if (associated(nobcfg)) then
    deallocate(nobcfg)
    nullify(nobcfg)
  end if
  if (associated(nobptr)) then
    deallocate(nobptr)
    nullify(nobptr)
  end if
  if (associated(nobptr2)) then
    deallocate(nobptr2)
    nullify(nobptr2)
  end if
  if (associated(nobptr3)) then
    deallocate(nobptr3)
    nullify(nobptr3)
  end if
  if (associated(nobtyp)) then
    deallocate(nobtyp)
    nullify(nobtyp)
  end if
  if (associated(nobsmodeat)) then
    deallocate(nobsmodeat)
    nullify(nobsmodeat)
  end if
  if (associated(nobsmodecfg)) then
    deallocate(nobsmodecfg)
    nullify(nobsmodecfg)
  end if
  if (associated(fcalc)) then
    deallocate(fcalc)
    nullify(fcalc)
  end if
  if (associated(fcalcoriginal)) then
    deallocate(fcalcoriginal)
    nullify(fcalcoriginal)
  end if
  if (associated(fgrad)) then
    deallocate(fgrad)
    nullify(fgrad)
  end if
  if (associated(fgradweight)) then
    deallocate(fgradweight)
    nullify(fgradweight)
  end if
  if (associated(finfo)) then
    deallocate(finfo)
    nullify(finfo)
  end if
  if (associated(fstrain)) then
    deallocate(fstrain)
    nullify(fstrain)
  end if
  if (associated(fstrainweight)) then
    deallocate(fstrainweight)
    nullify(fstrainweight)
  end if
  if (associated(fobs)) then
    deallocate(fobs)
    nullify(fobs)
  end if
  if (associated(fobsmode)) then
    deallocate(fobsmode)
    nullify(fobsmode)
  end if
  if (associated(fobsmodefreq)) then
    deallocate(fobsmodefreq)
    nullify(fobsmodefreq)
  end if
  if (associated(fobsmodeover)) then
    deallocate(fobsmodeover)
    nullify(fobsmodeover)
  end if
  if (associated(fparameter)) then
    deallocate(fparameter)
    nullify(fparameter)
  end if
  if (associated(freaction)) then
    deallocate(freaction)
    nullify(freaction)
  end if
  if (associated(weight)) then
    deallocate(weight)
    nullify(weight)
  end if

  !
  ! module one
  !
  if (associated(symbol1)) then
    deallocate(symbol1)
    nullify(symbol1)
  end if
  if (associated(nptyp11)) then
    deallocate(nptyp11)
    nullify(nptyp11)
  end if
  if (associated(nspec11)) then
    deallocate(nspec11)
    nullify(nspec11)
  end if
  if (associated(onepot)) then
    deallocate(onepot)
    nullify(onepot)
  end if


  !
  ! module optimisation
  !
  if (associated(lopf)) then
    deallocate(lopf)
    nullify(lopf)
  end if
  if (associated(rmode)) then
    deallocate(rmode)
    nullify(rmode)
  end if
  if (associated(noptatptr)) then
    deallocate(noptatptr)
    nullify(noptatptr)
  end if
  if (associated(noptatrptr)) then
    deallocate(noptatrptr)
    nullify(noptatrptr)
  end if
  if (associated(noptatlocptr)) then
    deallocate(noptatlocptr)
    nullify(noptatlocptr)
  end if
  if (associated(noptatlocrptr)) then
    deallocate(noptatlocrptr)
    nullify(noptatlocrptr)
  end if

  !
  ! module parallel
  !
  if (associated(atom2local)) then
    deallocate(atom2local)
    nullify(atom2local)
  end if
  if (associated(atom2locala)) then
    deallocate(atom2locala)
    nullify(atom2locala)
  end if
  if (associated(atom2localv)) then
    deallocate(atom2localv)
    nullify(atom2localv)
  end if
  if (associated(atom2node)) then
    deallocate(atom2node)
    nullify(atom2node)
  end if
  if (associated(atom2nodea)) then
    deallocate(atom2nodea)
    nullify(atom2nodea)
  end if
  if (associated(atom2nodev)) then
    deallocate(atom2nodev)
    nullify(atom2nodev)
  end if
  if (associated(node2atom)) then
    deallocate(node2atom)
    nullify(node2atom)
  end if
  if (associated(node2atoma)) then
    deallocate(node2atoma)
    nullify(node2atoma)
  end if
  if (associated(node2atomv)) then
    deallocate(node2atomv)
    nullify(node2atomv)
  end if
  if (associated(npts2local)) then
    deallocate(npts2local)
    nullify(npts2local)
  end if
  if (associated(npts2node)) then
    deallocate(npts2node)
    nullify(npts2node)
  end if
  if (associated(node2pts)) then
    deallocate(node2pts)
    nullify(node2pts)
  end if
  if (associated(nvar2local)) then
    deallocate(nvar2local)
    nullify(nvar2local)
  end if
  if (associated(nvar2node)) then
    deallocate(nvar2node)
    nullify(nvar2node)
  end if
  if (associated(node2var)) then
    deallocate(node2var)
    nullify(node2var)
  end if
  if (associated(ncoonnodeptr)) then
    deallocate(ncoonnodeptr)
    nullify(ncoonnodeptr)
  end if
  if (associated(nshonnodeptr)) then
    deallocate(nshonnodeptr)
    nullify(nshonnodeptr)
  end if
  if (associated(reg12node)) then
    deallocate(reg12node)
    nullify(reg12node)
  end if
  if (associated(reg12local)) then
    deallocate(reg12local)
    nullify(reg12local)
  end if
  if (associated(node2reg1)) then
    deallocate(node2reg1)
    nullify(node2reg1)
  end if

  !
  ! 07.2024 WKJEE
  !
  ! As a part of module parallel <MPI_comm>
  !
  ! * CALL BLACS_GRIDINIT(CONTEXT): 'initcomm.F90', which never freed.
  !   CONTEXT: typically a MPI_Comm handle
  !   (i.e., internal sub-communicator allocation results in communicator leaking)
  !   - possible error
  !     ...
  !     MPIR_Get_contextid_sparse_group(610): Too many communicators (0/2048 free on this process; ignore_id=0)
  !     ...
  ! * CALL BLACS_GRIDEXIT(CONTEXT):
  !   CONTEXT consume resources, and therefore CONTEXT should be released when no longer needed. 
  !   After the freeing of a context, the context no longer exists,
  !   * * * and its handle may be re-used if new contexts are defined. * * *
  !   Note. CALL BLACS_EXIT(CONTEXT): this will destroy CONTEXT, and the CONTEXT no longer available.
  !   (i.e., the MPI_Comm passed to Context will no longer be available)
  !
  call BLACS_GRIDEXIT(iBlacsContext) ! 01.07.2024 does not fix context leaking error

  !
  ! module partial
  !
  if (associated(ibocptr)) then
    deallocate(ibocptr)
    nullify(ibocptr)
  end if
  if (associated(ibocshptr)) then
    deallocate(ibocshptr)
    nullify(ibocshptr)
  end if
  if (associated(iocptr)) then
    deallocate(iocptr)
    nullify(iocptr)
  end if
  if (associated(iocshptr)) then
    deallocate(iocshptr)
    nullify(iocshptr)
  end if

  !
  ! module phononatoms
  !
  if (associated(nphonatptr)) then
    deallocate(nphonatptr)
    nullify(nphonatptr)
  end if
  if (associated(nphonatcptr)) then
    deallocate(nphonatcptr)
    nullify(nphonatcptr)
  end if
  if (associated(nphonatsptr)) then
    deallocate(nphonatsptr)
    nullify(nphonatsptr)
  end if
  if (associated(nphonatmptr)) then
    deallocate(nphonatmptr)
    nullify(nphonatmptr)
  end if
  if (associated(nphonatrptr)) then
    deallocate(nphonatrptr)
    nullify(nphonatrptr)
  end if
  if (associated(nphonatrcptr)) then
    deallocate(nphonatrcptr)
    nullify(nphonatrcptr)
  end if
  if (associated(nphonatrsptr)) then
    deallocate(nphonatrsptr)
    nullify(nphonatrsptr)
  end if
  if (associated(nphonatrmptr)) then
    deallocate(nphonatrmptr)
    nullify(nphonatrmptr)
  end if
  if (associated(nphonatonnodeptr)) then
    deallocate(nphonatonnodeptr)
    nullify(nphonatonnodeptr)
  end if
  if (associated(nphonatonnodecptr)) then
    deallocate(nphonatonnodecptr)
    nullify(nphonatonnodecptr)
  end if
  if (associated(nphonatonnodesptr)) then
    deallocate(nphonatonnodesptr)
    nullify(nphonatonnodesptr)
  end if
  if (associated(nphonatonnodemptr)) then
    deallocate(nphonatonnodemptr)
    nullify(nphonatonnodemptr)
  end if
  if (associated(nphonatonnoderptr)) then
    deallocate(nphonatonnoderptr)
    nullify(nphonatonnoderptr)
  end if
  if (associated(nphonatonnodercptr)) then
    deallocate(nphonatonnodercptr)
    nullify(nphonatonnodercptr)
  end if
  if (associated(nphonatonnodersptr)) then
    deallocate(nphonatonnodersptr)
    nullify(nphonatonnodersptr)
  end if
  if (associated(nphonatonnodermptr)) then
    deallocate(nphonatonnodermptr)
    nullify(nphonatonnodermptr)
  end if

  !
  ! module plane
  !
  if (associated(planepotsymbol)) then
    deallocate(planepotsymbol)
    nullify(planepotsymbol)
  end if
  if (associated(nplanepotpower)) then
    deallocate(nplanepotpower)
    nullify(nplanepotpower)
  end if
  if (associated(nplanepottype)) then
    deallocate(nplanepottype)
    nullify(nplanepottype)
  end if
  if (associated(natplanepot)) then
    deallocate(natplanepot)
    nullify(natplanepot)
  end if
  if (associated(ntypplanepot)) then
    deallocate(ntypplanepot)
    nullify(ntypplanepot)
  end if
  if (associated(planepot)) then
    deallocate(planepot)
    nullify(planepot)
  end if
  if (associated(planepotrmin)) then
    deallocate(planepotrmin)
    nullify(planepotrmin)
  end if
  if (associated(planepotrmax)) then
    deallocate(planepotrmax)
    nullify(planepotrmax)
  end if

  !
  ! module polarise
  !
  if (associated(natpolspec)) then
    deallocate(natpolspec)
    nullify(natpolspec)
  end if
  if (associated(ntyppolspec)) then
    deallocate(ntyppolspec)
    nullify(ntyppolspec)
  end if
  if (associated(dpolar)) then
    deallocate(dpolar)
    nullify(dpolar)
  end if
  if (associated(dpolarmax)) then
    deallocate(dpolarmax)
    nullify(dpolarmax)
  end if
  if (associated(qpolar)) then
    deallocate(qpolar)
    nullify(qpolar)
  end if
  if (associated(dpolspec)) then
    deallocate(dpolspec)
    nullify(dpolspec)
  end if
  if (associated(dpolmaxspec)) then
    deallocate(dpolmaxspec)
    nullify(dpolmaxspec)
  end if
  if (associated(qpolspec)) then
    deallocate(qpolspec)
    nullify(qpolspec)
  end if


  !
  ! module potchange
  !
  if (associated(npchng)) then
    deallocate(npchng)
    nullify(npchng)
  end if


  !
  ! module potentialgrid
  !
  if (associated(nxpg)) then
    deallocate(nxpg)
    nullify(nxpg)
  end if
  if (associated(nypg)) then
    deallocate(nypg)
    nullify(nypg)
  end if
  if (associated(nzpg)) then
    deallocate(nzpg)
    nullify(nzpg)
  end if
  if (associated(xmaxpg)) then
    deallocate(xmaxpg)
    nullify(xmaxpg)
  end if
  if (associated(xminpg)) then
    deallocate(xminpg)
    nullify(xminpg)
  end if
  if (associated(ymaxpg)) then
    deallocate(ymaxpg)
    nullify(ymaxpg)
  end if
  if (associated(yminpg)) then
    deallocate(yminpg)
    nullify(yminpg)
  end if
  if (associated(zmaxpg)) then
    deallocate(zmaxpg)
    nullify(zmaxpg)
  end if
  if (associated(zminpg)) then
    deallocate(zminpg)
    nullify(zminpg)
  end if


  !
  ! module potentialinterpolation
  !
  if (associated(dRinterpolate)) then
    deallocate(dRinterpolate)
    nullify(dRinterpolate)
  end if
  if (associated(rdRinterpolate)) then
    deallocate(rdRinterpolate)
    nullify(rdRinterpolate)
  end if
  if (associated(FofR)) then
    deallocate(FofR)
    nullify(FofR)
  end if
  if (associated(dFofR)) then
    deallocate(dFofR)
    nullify(dFofR)
  end if


  !
  ! module potentialpoints, potentialsites, potentialxyz
  !
  if (associated(npotptcfg)) then
    deallocate(npotptcfg)
    nullify(npotptcfg)
  end if
  if (associated(xpotpt)) then
    deallocate(xpotpt)
    nullify(xpotpt)
  end if
  if (associated(ypotpt)) then
    deallocate(ypotpt)
    nullify(ypotpt)
  end if
  if (associated(zpotpt)) then
    deallocate(zpotpt)
    nullify(zpotpt)
  end if
  if (associated(vpotpt)) then
    deallocate(vpotpt)
    nullify(vpotpt)
  end if
  if (associated(npotsitescfg)) then
    deallocate(npotsitescfg)
    nullify(npotsitescfg)
  end if
  if (associated(npotsitecfg)) then
    deallocate(npotsitecfg)
    nullify(npotsitecfg)
  end if
  if (associated(lpotsitecartcfg)) then
    deallocate(lpotsitecartcfg)
    nullify(lpotsitecartcfg)
  end if
  if (associated(xpotsite)) then
    deallocate(xpotsite)
    nullify(xpotsite)
  end if
  if (associated(ypotsite)) then
    deallocate(ypotsite)
    nullify(ypotsite)
  end if
  if (associated(zpotsite)) then
    deallocate(zpotsite)
    nullify(zpotsite)
  end if
  if (associated(vpotsite)) then
    deallocate(vpotsite)
    nullify(vpotsite)
  end if
  if (associated(v2xyz)) then
    deallocate(v2xyz)
    nullify(v2xyz)
  end if
  if (associated(vx)) then
    deallocate(vx)
    nullify(vx)
  end if
  if (associated(vy)) then
    deallocate(vy)
    nullify(vy)
  end if
  if (associated(vz)) then
    deallocate(vz)
    nullify(vz)
  end if
  if (associated(v2xyz12)) then
    deallocate(v2xyz12)
    nullify(v2xyz12)
  end if
  if (associated(vx12)) then
    deallocate(vx12)
    nullify(vx12)
  end if
  if (associated(vy12)) then
    deallocate(vy12)
    nullify(vy12)
  end if
  if (associated(vz12)) then
    deallocate(vz12)
    nullify(vz12)
  end if

  !
  ! module projectdos
  !
  if (associated(nprojit)) then
    deallocate(nprojit)
    nullify(nprojit)
  end if
  if (associated(nprojcfg)) then
    deallocate(nprojcfg)
    nullify(nprojcfg)
  end if
  if (associated(nprojnat)) then
    deallocate(nprojnat)
    nullify(nprojnat)
  end if
  if (associated(nprojtyp)) then
    deallocate(nprojtyp)
    nullify(nprojtyp)
  end if
  if (associated(nprojdb)) then
    deallocate(nprojdb)
    nullify(nprojdb)
  end if
  if (associated(nprojdef)) then
    deallocate(nprojdef)
    nullify(nprojdef)
  end if
  if (associated(nprojptr)) then
    deallocate(nprojptr)
    nullify(nprojptr)
  end if

  !
  ! module properties
  !
  if (associated(ramanasus)) then
    deallocate(ramanasus)
    nullify(ramanasus)
  end if

  !
  ! module radial
  !
  if (associated(lradialcfg)) then
    deallocate(lradialcfg)
    nullify(lradialcfg)
  end if
  if (associated(radialKcfg)) then
    deallocate(radialKcfg)
    nullify(radialKcfg)
  end if
  if (associated(radialXYZcfg)) then
    deallocate(radialXYZcfg)
    nullify(radialXYZcfg)
  end if

  !
  ! module reaxFFdata
  !
  if (associated(symbolreaxFFspec)) then
    deallocate(symbolreaxFFspec)
    nullify(symbolreaxFFspec)
  end if
  if (associated(nreaxFFfixQspecptr)) then
    deallocate(nreaxFFfixQspecptr)
    nullify(nreaxFFfixQspecptr)
  end if
  if (associated(natreaxFFspec)) then
    deallocate(natreaxFFspec)
    nullify(natreaxFFspec)
  end if
  if (associated(ntypreaxFFspec)) then
    deallocate(ntypreaxFFspec)
    nullify(ntypreaxFFspec)
  end if
  if (associated(nreaxFFval3)) then
    deallocate(nreaxFFval3)
    nullify(nreaxFFval3)
  end if
  if (associated(lreaxFFbocorrect)) then
    deallocate(lreaxFFbocorrect)
    nullify(lreaxFFbocorrect)
  end if
  if (associated(lreaxFFmorseinput)) then
    deallocate(lreaxFFmorseinput)
    nullify(lreaxFFmorseinput)
  end if
  if (associated(lreaxFFpboOK)) then
    deallocate(lreaxFFpboOK)
    nullify(lreaxFFpboOK)
  end if
  if (associated(lreaxFFqfix)) then
    deallocate(lreaxFFqfix)
    nullify(lreaxFFqfix)
  end if
  if (associated(lreaxFFtorsinput)) then
    deallocate(lreaxFFtorsinput)
    nullify(lreaxFFtorsinput)
  end if
  if (associated(lreaxFFunder)) then
    deallocate(lreaxFFunder)
    nullify(lreaxFFunder)
  end if
  if (associated(qreaxFF)) then
    deallocate(qreaxFF)
    nullify(qreaxFF)
  end if
  if (associated(reaxFFr)) then
    deallocate(reaxFFr)
    nullify(reaxFFr)
  end if
  if (associated(reaxFFalpha)) then
    deallocate(reaxFFalpha)
    nullify(reaxFFalpha)
  end if
  if (associated(reaxFFeps)) then
    deallocate(reaxFFeps)
    nullify(reaxFFeps)
  end if
  if (associated(reaxFFrvdw)) then
    deallocate(reaxFFrvdw)
    nullify(reaxFFrvdw)
  end if
  if (associated(reaxFFgammaw)) then
    deallocate(reaxFFgammaw)
    nullify(reaxFFgammaw)
  end if
  if (associated(reaxFFpover)) then
    deallocate(reaxFFpover)
    nullify(reaxFFpover)
  end if
  if (associated(reaxFFpunder)) then
    deallocate(reaxFFpunder)
    nullify(reaxFFpunder)
  end if
  if (associated(reaxFFhincrement)) then
    deallocate(reaxFFhincrement)
    nullify(reaxFFhincrement)
  end if
  if (associated(reaxFFlp)) then
    deallocate(reaxFFlp)
    nullify(reaxFFlp)
  end if
  if (associated(reaxFFmorse)) then
    deallocate(reaxFFmorse)
    nullify(reaxFFmorse)
  end if
  if (associated(reaxFFoc1)) then
    deallocate(reaxFFoc1)
    nullify(reaxFFoc1)
  end if
  if (associated(reaxFFoc2)) then
    deallocate(reaxFFoc2)
    nullify(reaxFFoc2)
  end if
  if (associated(reaxFFuc1)) then
    deallocate(reaxFFuc1)
    nullify(reaxFFuc1)
  end if
  if (associated(reaxFFpboc)) then
    deallocate(reaxFFpboc)
    nullify(reaxFFpboc)
  end if
  if (associated(reaxFFval)) then
    deallocate(reaxFFval)
    nullify(reaxFFval)
  end if
  if (associated(reaxFFval1)) then
    deallocate(reaxFFval1)
    nullify(reaxFFval1)
  end if
  if (associated(reaxFFval3)) then
    deallocate(reaxFFval3)
    nullify(reaxFFval3)
  end if
  if (associated(reaxFFconj3)) then
    deallocate(reaxFFconj3)
    nullify(reaxFFconj3)
  end if
  if (associated(reaxFFhb3)) then
    deallocate(reaxFFhb3)
    nullify(reaxFFhb3)
  end if
  if (associated(reaxFFpen2)) then
    deallocate(reaxFFpen2)
    nullify(reaxFFpen2)
  end if
  if (associated(reaxFFpen3)) then
    deallocate(reaxFFpen3)
    nullify(reaxFFpen3)
  end if
  if (associated(reaxFFtor4)) then
    deallocate(reaxFFtor4)
    nullify(reaxFFtor4)
  end if
  if (associated(reaxFFDe)) then
    deallocate(reaxFFDe)
    nullify(reaxFFDe)
  end if
  if (associated(reaxFFpbe)) then
    deallocate(reaxFFpbe)
    nullify(reaxFFpbe)
  end if
  if (associated(reaxFFpbo)) then
    deallocate(reaxFFpbo)
    nullify(reaxFFpbo)
  end if
  if (associated(reaxFFDeVDW)) then
    deallocate(reaxFFDeVDW)
    nullify(reaxFFDeVDW)
  end if
  if (associated(reaxFFalphaVDW)) then
    deallocate(reaxFFalphaVDW)
    nullify(reaxFFalphaVDW)
  end if
  if (associated(reaxFFr0VDW)) then
    deallocate(reaxFFr0VDW)
    nullify(reaxFFr0VDW)
  end if
  if (associated(reaxFFgammaVDW)) then
    deallocate(reaxFFgammaVDW)
    nullify(reaxFFgammaVDW)
  end if
  if (associated(reaxFFgammaQ)) then
    deallocate(reaxFFgammaQ)
    nullify(reaxFFgammaQ)
  end if
  if (associated(reaxFFrmax)) then
    deallocate(reaxFFrmax)
    nullify(reaxFFrmax)
  end if
  if (associated(reaxFFrmaxpair)) then
    deallocate(reaxFFrmaxpair)
    nullify(reaxFFrmaxpair)
  end if
  if (associated(reaxFFchi)) then
    deallocate(reaxFFchi)
    nullify(reaxFFchi)
  end if
  if (associated(reaxFFgamma)) then
    deallocate(reaxFFgamma)
    nullify(reaxFFgamma)
  end if
  if (associated(reaxFFmu)) then
    deallocate(reaxFFmu)
    nullify(reaxFFmu)
  end if
  if (associated(reaxFFq0)) then
    deallocate(reaxFFq0)
    nullify(reaxFFq0)
  end if
  if (associated(reaxFFqfix)) then
    deallocate(reaxFFqfix)
    nullify(reaxFFqfix)
  end if
  if (associated(reaxFFshell)) then
    deallocate(reaxFFshell)
    nullify(reaxFFshell)
  end if

  !
  ! module realvectors
  !
  if (associated(cellindex)) then
    deallocate(cellindex)
    nullify(cellindex)
  end if
  if (associated(nbotype)) then
    deallocate(nbotype)
    nullify(nbotype)
  end if
  if (associated(nbotype2)) then
    deallocate(nbotype2)
    nullify(nbotype2)
  end if
  if (associated(lbonded)) then
    deallocate(lbonded)
    nullify(lbonded)
  end if
  if (associated(l2bonds)) then
    deallocate(l2bonds)
    nullify(l2bonds)
  end if
  if (associated(l3bonds)) then
    deallocate(l3bonds)
    nullify(l3bonds)
  end if
  if (associated(lptrmol)) then
    deallocate(lptrmol)
    nullify(lptrmol)
  end if
  if (associated(deriv)) then
    deallocate(deriv)
    nullify(deriv)
  end if
  if (associated(deriv2)) then
    deallocate(deriv2)
    nullify(deriv2)
  end if
  if (associated(deriv3)) then
    deallocate(deriv3)
    nullify(deriv3)
  end if
  if (associated(derive0)) then
    deallocate(derive0)
    nullify(derive0)
  end if
  if (associated(derive)) then
    deallocate(derive)
    nullify(derive)
  end if
  if (associated(derive2)) then
    deallocate(derive2)
    nullify(derive2)
  end if
  if (associated(derive3)) then
    deallocate(derive3)
    nullify(derive3)
  end if
  if (associated(derivqd)) then
    deallocate(derivqd)
    nullify(derivqd)
  end if
  if (associated(derivqd2)) then
    deallocate(derivqd2)
    nullify(derivqd2)
  end if
  if (associated(derivqd3)) then
    deallocate(derivqd3)
    nullify(derivqd3)
  end if
  if (associated(dist)) then
    deallocate(dist)
    nullify(dist)
  end if
  if (associated(dist2)) then
    deallocate(dist2)
    nullify(dist2)
  end if
  if (associated(dist3)) then
    deallocate(dist3)
    nullify(dist3)
  end if
  if (associated(d0i)) then
    deallocate(d0i)
    nullify(d0i)
  end if
  if (associated(d0j)) then
    deallocate(d0j)
    nullify(d0j)
  end if
  if (associated(d1i)) then
    deallocate(d1i)
    nullify(d1i)
  end if
  if (associated(d1j)) then
    deallocate(d1j)
    nullify(d1j)
  end if
  if (associated(d2i2)) then
    deallocate(d2i2)
    nullify(d2i2)
  end if
  if (associated(d2ij)) then
    deallocate(d2ij)
    nullify(d2ij)
  end if
  if (associated(d2j2)) then
    deallocate(d2j2)
    nullify(d2j2)
  end if
  if (associated(rderiv)) then
    deallocate(rderiv)
    nullify(rderiv)
  end if
  if (associated(rpd)) then
    deallocate(rpd)
    nullify(rpd)
  end if
  if (associated(dr2ds)) then
    deallocate(dr2ds)
    nullify(dr2ds)
  end if
  if (associated(d2r2ds2)) then
    deallocate(d2r2ds2)
    nullify(d2r2ds2)
  end if
  if (associated(d2r2dsdx)) then
    deallocate(d2r2dsdx)
    nullify(d2r2dsdx)
  end if
  if (associated(d2r2dx2)) then
    deallocate(d2r2dx2)
    nullify(d2r2dx2)
  end if
  if (associated(rtrm1)) then
    deallocate(rtrm1)
    nullify(rtrm1)
  end if
  if (associated(rtrm2)) then
    deallocate(rtrm2)
    nullify(rtrm2)
  end if
  if (associated(rtrm3)) then
    deallocate(rtrm3)
    nullify(rtrm3)
  end if
  if (associated(rtrm32)) then
    deallocate(rtrm32)
    nullify(rtrm32)
  end if
  if (associated(xtmp)) then
    deallocate(xtmp)
    nullify(xtmp)
  end if
  if (associated(ytmp)) then
    deallocate(ytmp)
    nullify(ytmp)
  end if
  if (associated(ztmp)) then
    deallocate(ztmp)
    nullify(ztmp)
  end if
  if (associated(xtmp2)) then
    deallocate(xtmp2)
    nullify(xtmp2)
  end if
  if (associated(ytmp2)) then
    deallocate(ytmp2)
    nullify(ytmp2)
  end if
  if (associated(ztmp2)) then
    deallocate(ztmp2)
    nullify(ztmp2)
  end if
  if (associated(xtmp3)) then
    deallocate(xtmp3)
    nullify(xtmp3)
  end if
  if (associated(ytmp3)) then
    deallocate(ytmp3)
    nullify(ytmp3)
  end if
  if (associated(ztmp3)) then
    deallocate(ztmp3)
    nullify(ztmp3)
  end if

  !
  ! module region2a
  !
  if (associated(nr2a)) then
    deallocate(nr2a)
    nullify(nr2a)
  end if
  if (associated(ntr2a)) then
    deallocate(ntr2a)
    nullify(ntr2a)
  end if
  if (associated(nmr2a)) then
    deallocate(nmr2a)
    nullify(nmr2a)
  end if
  if (associated(nmir2a)) then
    deallocate(nmir2a)
    nullify(nmir2a)
  end if
  if (associated(nps)) then
    deallocate(nps)
    nullify(nps)
  end if
  if (associated(ndsptr2a)) then
    deallocate(ndsptr2a)
    nullify(ndsptr2a)
  end if
  if (associated(ndeqv2a)) then
    deallocate(ndeqv2a)
    nullify(ndeqv2a)
  end if
  if (associated(ndrel2a)) then
    deallocate(ndrel2a)
    nullify(ndrel2a)
  end if
  if (associated(ndrelop2a)) then
    deallocate(ndrelop2a)
    nullify(ndrelop2a)
  end if
  if (associated(ldbr2a)) then
    deallocate(ldbr2a)
    nullify(ldbr2a)
  end if
  if (associated(dscrhor2d)) then
    deallocate(dscrhor2d)
    nullify(dscrhor2d)
  end if
  if (associated(dscrhor2p)) then
    deallocate(dscrhor2p)
    nullify(dscrhor2p)
  end if
  if (associated(xdis)) then
    deallocate(xdis)
    nullify(xdis)
  end if
  if (associated(ydis)) then
    deallocate(ydis)
    nullify(ydis)
  end if
  if (associated(zdis)) then
    deallocate(zdis)
    nullify(zdis)
  end if
  if (associated(xr2a)) then
    deallocate(xr2a)
    nullify(xr2a)
  end if
  if (associated(yr2a)) then
    deallocate(yr2a)
    nullify(yr2a)
  end if
  if (associated(zr2a)) then
    deallocate(zr2a)
    nullify(zr2a)
  end if
  if (associated(qr2a)) then
    deallocate(qr2a)
    nullify(qr2a)
  end if
  if (associated(or2a)) then
    deallocate(or2a)
    nullify(or2a)
  end if
  if (associated(rr2a)) then
    deallocate(rr2a)
    nullify(rr2a)
  end if

  !
  ! module scan
  !
  if (associated(ncscan)) then
    deallocate(ncscan)
    nullify(ncscan)
  end if
  if (associated(ntran)) then
    deallocate(ntran)
    nullify(ntran)
  end if
  if (associated(lcscanstrain)) then
    deallocate(lcscanstrain)
    nullify(lcscanstrain)
  end if
  if (associated(ltranat)) then
    deallocate(ltranat)
    nullify(ltranat)
  end if
  if (associated(ltranatminus)) then
    deallocate(ltranatminus)
    nullify(ltranatminus)
  end if
  if (associated(ltrannoise)) then
    deallocate(ltrannoise)
    nullify(ltrannoise)
  end if
  if (associated(ltrantherm)) then
    deallocate(ltrantherm)
    nullify(ltrantherm)
  end if
  if (associated(trannoise)) then
    deallocate(trannoise)
    nullify(trannoise)
  end if
  if (associated(trantherm)) then
    deallocate(trantherm)
    nullify(trantherm)
  end if
  if (associated(cscan)) then
    deallocate(cscan)
    nullify(cscan)
  end if
  if (associated(xtran)) then
    deallocate(xtran)
    nullify(xtran)
  end if
  if (associated(ytran)) then
    deallocate(ytran)
    nullify(ytran)
  end if
  if (associated(ztran)) then
    deallocate(ztran)
    nullify(ztran)
  end if

  !
  ! module scatterdata
  !
  if (associated(n_qpoint)) then
    deallocate(n_qpoint)
    nullify(n_qpoint)
  end if
  if (associated(q_ordinate)) then
    deallocate(q_ordinate)
    nullify(q_ordinate)
  end if
  if (associated(Hold_Q)) then
    deallocate(Hold_Q)
    nullify(Hold_Q)
  end if
  if (associated(Hold_smq)) then
    deallocate(Hold_smq)
    nullify(Hold_smq)
  end if
  if (associated(Hold_tau)) then
    deallocate(Hold_tau)
    nullify(Hold_tau)
  end if
  if (associated(Qvector)) then
    deallocate(Qvector)
    nullify(Qvector)
  end if
  if (associated(scatlencoh)) then
    deallocate(scatlencoh)
    nullify(scatlencoh)
  end if
  if (associated(scatleninc)) then
    deallocate(scatleninc)
    nullify(scatleninc)
  end if
  if (associated(sofomega)) then
    deallocate(sofomega)
    nullify(sofomega)
  end if
  if (associated(sofomega_fit)) then
    deallocate(sofomega_fit)
    nullify(sofomega_fit)
  end if
  if (associated(tauvector)) then
    deallocate(tauvector)
    nullify(tauvector)
  end if

  !
  ! module shells
  !
  if (associated(nbsptr)) then
    deallocate(nbsptr)
    nullify(nbsptr)
  end if
  if (associated(ncsptr)) then
    deallocate(ncsptr)
    nullify(ncsptr)
  end if
  if (associated(ncoptr)) then
    deallocate(ncoptr)
    nullify(ncoptr)
  end if
  if (associated(ncoshptr)) then
    deallocate(ncoshptr)
    nullify(ncoshptr)
  end if
  if (associated(nshptr)) then
    deallocate(nshptr)
    nullify(nshptr)
  end if
  if (associated(natratiomspec)) then
    deallocate(natratiomspec)
    nullify(natratiomspec)
  end if
  if (associated(ntypratiomspec)) then
    deallocate(ntypratiomspec)
    nullify(ntypratiomspec)
  end if
  if (associated(ratiom)) then
    deallocate(ratiom)
    nullify(ratiom)
  end if
  if (associated(ratiomspec)) then
    deallocate(ratiomspec)
    nullify(ratiomspec)
  end if
  if (associated(csvector)) then
    deallocate(csvector)
    nullify(csvector)
  end if


  !
  ! module shellextrapolation
  !
  if (associated(xshellsave)) then
    deallocate(xshellsave)
    nullify(xshellsave)
  end if
  if (associated(yshellsave)) then
    deallocate(yshellsave)
    nullify(yshellsave)
  end if
  if (associated(zshellsave)) then
    deallocate(zshellsave)
    nullify(zshellsave)
  end if

  !
  ! module shifts
  !
  if (associated(nshcfg)) then
    deallocate(nshcfg)
    nullify(nshcfg)
  end if
  if (associated(shift)) then
    deallocate(shift)
    nullify(shift)
  end if
  if (associated(shscalecfg)) then
    deallocate(shscalecfg)
    nullify(shscalecfg)
  end if

  !
  ! module six
  !
  if (associated(symbol6)) then
    deallocate(symbol6)
    nullify(symbol6)
  end if
  if (associated(icell61)) then
    deallocate(icell61)
    nullify(icell61)
  end if
  if (associated(icell62)) then
    deallocate(icell62)
    nullify(icell62)
  end if
  if (associated(icell63)) then
    deallocate(icell63)
    nullify(icell63)
  end if
  if (associated(icell64)) then
    deallocate(icell64)
    nullify(icell64)
  end if
  if (associated(icell65)) then
    deallocate(icell65)
    nullify(icell65)
  end if
  if (associated(ijind)) then
    deallocate(ijind)
    nullify(ijind)
  end if
  if (associated(klind)) then
    deallocate(klind)
    nullify(klind)
  end if
  if (associated(mnind)) then
    deallocate(mnind)
    nullify(mnind)
  end if
  if (associated(mmsexc)) then
    deallocate(mmsexc)
    nullify(mmsexc)
  end if
  if (associated(n6botype)) then
    deallocate(n6botype)
    nullify(n6botype)
  end if
  if (associated(nsixptr)) then
    deallocate(nsixptr)
    nullify(nsixptr)
  end if
  if (associated(nsixty)) then
    deallocate(nsixty)
    nullify(nsixty)
  end if
  if (associated(nsptyp1)) then
    deallocate(nsptyp1)
    nullify(nsptyp1)
  end if
  if (associated(nsptyp2)) then
    deallocate(nsptyp2)
    nullify(nsptyp2)
  end if
  if (associated(nsptyp3)) then
    deallocate(nsptyp3)
    nullify(nsptyp3)
  end if
  if (associated(nsptyp4)) then
    deallocate(nsptyp4)
    nullify(nsptyp4)
  end if
  if (associated(nsptyp5)) then
    deallocate(nsptyp5)
    nullify(nsptyp5)
  end if
  if (associated(nsptyp6)) then
    deallocate(nsptyp6)
    nullify(nsptyp6)
  end if
  if (associated(nsspec1)) then
    deallocate(nsspec1)
    nullify(nsspec1)
  end if
  if (associated(nsspec2)) then
    deallocate(nsspec2)
    nullify(nsspec2)
  end if
  if (associated(nsspec3)) then
    deallocate(nsspec3)
    nullify(nsspec3)
  end if
  if (associated(nsspec4)) then
    deallocate(nsspec4)
    nullify(nsspec4)
  end if
  if (associated(nsspec5)) then
    deallocate(nsspec5)
    nullify(nsspec5)
  end if
  if (associated(nsspec6)) then
    deallocate(nsspec6)
    nullify(nsspec6)
  end if
  if (associated(npsix)) then
    deallocate(npsix)
    nullify(npsix)
  end if
  if (associated(lsintra)) then
    deallocate(lsintra)
    nullify(lsintra)
  end if
  if (associated(lsinter)) then
    deallocate(lsinter)
    nullify(lsinter)
  end if
  if (associated(ltib6body)) then
    deallocate(ltib6body)
    nullify(ltib6body)
  end if
  if (associated(ltif6body)) then
    deallocate(ltif6body)
    nullify(ltif6body)
  end if
  if (associated(sixk)) then
    deallocate(sixk)
    nullify(sixk)
  end if
  if (associated(six1)) then
    deallocate(six1)
    nullify(six1)
  end if
  if (associated(six2)) then
    deallocate(six2)
    nullify(six2)
  end if
  if (associated(six3)) then
    deallocate(six3)
    nullify(six3)
  end if
  if (associated(six4)) then
    deallocate(six4)
    nullify(six4)
  end if
  if (associated(six5)) then
    deallocate(six5)
    nullify(six5)
  end if

  !
  ! module spatial
  !
  if (associated(natomcell)) then
    deallocate(natomcell)
    nullify(natomcell)
  end if
  if (associated(natomnodeptr)) then
    deallocate(natomnodeptr)
    nullify(natomnodeptr)
  end if
  if (associated(ncellnodeptr)) then
    deallocate(ncellnodeptr)
    nullify(ncellnodeptr)
  end if
  if (associated(nspcellat)) then
    deallocate(nspcellat)
    nullify(nspcellat)
  end if
  if (associated(nspcellat1ptr)) then
    deallocate(nspcellat1ptr)
    nullify(nspcellat1ptr)
  end if
  if (associated(nspcellatptr)) then
    deallocate(nspcellatptr)
    nullify(nspcellatptr)
  end if
  if (associated(nspcell2atptr)) then
    deallocate(nspcell2atptr)
    nullify(nspcell2atptr)
  end if
  if (associated(nspcellatptrcell)) then
    deallocate(nspcellatptrcell)
    nullify(nspcellatptrcell)
  end if
  if (associated(lbuffercell)) then
    deallocate(lbuffercell)
    nullify(lbuffercell)
  end if
  if (associated(xinbox)) then
    deallocate(xinbox)
    nullify(xinbox)
  end if
  if (associated(yinbox)) then
    deallocate(yinbox)
    nullify(yinbox)
  end if
  if (associated(zinbox)) then
    deallocate(zinbox)
    nullify(zinbox)
  end if
  if (associated(xfinbox)) then
    deallocate(xfinbox)
    nullify(xfinbox)
  end if
  if (associated(yfinbox)) then
    deallocate(yfinbox)
    nullify(yfinbox)
  end if
  if (associated(zfinbox)) then
    deallocate(zfinbox)
    nullify(zfinbox)
  end if

  !
  ! module spatialbo
  !
  if (associated(natomcellbo)) then
    deallocate(natomcellbo)
    nullify(natomcellbo)
  end if
  if (associated(natomnodeptrbo)) then
    deallocate(natomnodeptrbo)
    nullify(natomnodeptrbo)
  end if
  if (associated(ncellnodeptrbo)) then
    deallocate(ncellnodeptrbo)
    nullify(ncellnodeptrbo)
  end if
  if (associated(nspcellatbo)) then
    deallocate(nspcellatbo)
    nullify(nspcellatbo)
  end if
  if (associated(nspcellat1ptrbo)) then
    deallocate(nspcellat1ptrbo)
    nullify(nspcellat1ptrbo)
  end if
  if (associated(nspcellatptrbo)) then
    deallocate(nspcellatptrbo)
    nullify(nspcellatptrbo)
  end if
  if (associated(nspcellatptrcellbo)) then
    deallocate(nspcellatptrcellbo)
    nullify(nspcellatptrcellbo)
  end if
  if (associated(lbuffercellbo)) then
    deallocate(lbuffercellbo)
    nullify(lbuffercellbo)
  end if
  if (associated(xinboxbo)) then
    deallocate(xinboxbo)
    nullify(xinboxbo)
  end if
  if (associated(yinboxbo)) then
    deallocate(yinboxbo)
    nullify(yinboxbo)
  end if
  if (associated(zinboxbo)) then
    deallocate(zinboxbo)
    nullify(zinboxbo)
  end if
  if (associated(xfinboxbo)) then
    deallocate(xfinboxbo)
    nullify(xfinboxbo)
  end if
  if (associated(yfinboxbo)) then
    deallocate(yfinboxbo)
    nullify(yfinboxbo)
  end if
  if (associated(zfinboxbo)) then
    deallocate(zfinboxbo)
    nullify(zfinboxbo)
  end if

  !
  ! module species
  !
  if (associated(symspec)) then
    deallocate(symspec)
    nullify(symspec)
  end if
  if (associated(natspec)) then
    deallocate(natspec)
    nullify(natspec)
  end if
  if (associated(numofspec)) then
    deallocate(numofspec)
    nullify(numofspec)
  end if
  if (associated(ntypspec)) then
    deallocate(ntypspec)
    nullify(ntypspec)
  end if
  if (associated(lbrspec)) then
    deallocate(lbrspec)
    nullify(lbrspec)
  end if
  if (associated(ldefshspec)) then
    deallocate(ldefshspec)
    nullify(ldefshspec)
  end if
  if (associated(linspec)) then
    deallocate(linspec)
    nullify(linspec)
  end if
  if (associated(lgastinspec)) then
    deallocate(lgastinspec)
    nullify(lgastinspec)
  end if
  if (associated(lgastinlibspec)) then
    deallocate(lgastinlibspec)
    nullify(lgastinlibspec)
  end if
  if (associated(lqinspec)) then
    deallocate(lqinspec)
    nullify(lqinspec)
  end if
  if (associated(lmassinspec)) then
    deallocate(lmassinspec)
    nullify(lmassinspec)
  end if
  if (associated(lnmrinspec)) then
    deallocate(lnmrinspec)
    nullify(lnmrinspec)
  end if
  if (associated(lspininspec)) then
    deallocate(lspininspec)
    nullify(lspininspec)
  end if
  if (associated(lvdwinspec)) then
    deallocate(lvdwinspec)
    nullify(lvdwinspec)
  end if
  if (associated(ltibqspec)) then
    deallocate(ltibqspec)
    nullify(ltibqspec)
  end if
  if (associated(ltifqspec)) then
    deallocate(ltifqspec)
    nullify(ltifqspec)
  end if
  if (associated(lmask)) then
    deallocate(lmask)
    nullify(lmask)
  end if
  if (associated(c6spec)) then
    deallocate(c6spec)
    nullify(c6spec)
  end if
  if (associated(gastspec)) then
    deallocate(gastspec)
    nullify(gastspec)
  end if
  if (associated(qlspec)) then
    deallocate(qlspec)
    nullify(qlspec)
  end if
  if (associated(massspec)) then
    deallocate(massspec)
    nullify(massspec)
  end if
  if (associated(nmrspec)) then
    deallocate(nmrspec)
    nullify(nmrspec)
  end if
  if (associated(radspec)) then
    deallocate(radspec)
    nullify(radspec)
  end if
  if (associated(spinspec)) then
    deallocate(spinspec)
    nullify(spinspec)
  end if
  if (associated(vdwspec)) then
    deallocate(vdwspec)
    nullify(vdwspec)
  end if

  !
  ! module splinedata
  !
  if (associated(nsplpt)) then
    deallocate(nsplpt)
    nullify(nsplpt)
  end if
  if (associated(nsplty)) then
    deallocate(nsplty)
    nullify(nsplty)
  end if
  if (associated(d1f)) then
    deallocate(d1f)
    nullify(d1f)
  end if
  if (associated(d2f)) then
    deallocate(d2f)
    nullify(d2f)
  end if
  if (associated(splf)) then
    deallocate(splf)
    nullify(splf)
  end if
  if (associated(splr)) then
    deallocate(splr)
    nullify(splr)
  end if

  !
  ! module spme
  !
  if (associated(nqkgrid)) then
    deallocate(nqkgrid)
    nullify(nqkgrid)
  end if

  !
  ! module sutton
  !
  if (associated(scrho)) then
    deallocate(scrho)
    nullify(scrho)
  end if
  if (associated(scrho12)) then
    deallocate(scrho12)
    nullify(scrho12)
  end if

  !
  ! module symmetry
  !
  if (associated(hmssg)) then
    deallocate(hmssg)
    nullify(hmssg)
  end if
  if (associated(hmssgio)) then
    deallocate(hmssgio)
    nullify(hmssgio)
  end if
  if (associated(ifhr)) then
    deallocate(ifhr)
    nullify(ifhr)
  end if
  if (associated(iflags)) then
    deallocate(iflags)
    nullify(iflags)
  end if
  if (associated(ifso)) then
    deallocate(ifso)
    nullify(ifso)
  end if
  if (associated(iperm)) then
    deallocate(iperm)
    nullify(iperm)
  end if
  if (associated(nccscfg)) then
    deallocate(nccscfg)
    nullify(nccscfg)
  end if
  if (associated(nspcg)) then
    deallocate(nspcg)
    nullify(nspcg)
  end if
  if (associated(nspcgp)) then
    deallocate(nspcgp)
    nullify(nspcgp)
  end if
  if (associated(ivso)) then
    deallocate(ivso)
    nullify(ivso)
  end if
  if (associated(symnocfg)) then
    deallocate(symnocfg)
    nullify(symnocfg)
  end if
  if (associated(lsymset)) then
    deallocate(lsymset)
    nullify(lsymset)
  end if
  if (associated(ngocfg)) then
    deallocate(ngocfg)
    nullify(ngocfg)
  end if
  if (associated(ropcfg)) then
    deallocate(ropcfg)
    nullify(ropcfg)
  end if
  if (associated(vitcfg)) then
    deallocate(vitcfg)
    nullify(vitcfg)
  end if

  !
  ! module thermalcond
  !
  if (associated(lomega_af_in)) then
    deallocate(lomega_af_in)
    nullify(lomega_af_in)
  end if
  if (associated(omega_af)) then
    deallocate(omega_af)
    nullify(omega_af)
  end if
  if (associated(v_s_cfg)) then
    deallocate(v_s_cfg)
    nullify(v_s_cfg)
  end if
  if (associated(v_p_cfg)) then
    deallocate(v_p_cfg)
    nullify(v_p_cfg)
  end if
  if (associated(B_pr_cfg)) then
    deallocate(B_pr_cfg)
    nullify(B_pr_cfg)
  end if

  !
  ! module m_three
  !
  if (associated(symbol3)) then
    deallocate(symbol3)
    nullify(symbol3)
  end if
  if (associated(icell31)) then
    deallocate(icell31)
    nullify(icell31)
  end if
  if (associated(icell32)) then
    deallocate(icell32)
    nullify(icell32)
  end if
  if (associated(i3ind)) then
    deallocate(i3ind)
    nullify(i3ind)
  end if
  if (associated(j3ind)) then
    deallocate(j3ind)
    nullify(j3ind)
  end if
  if (associated(k3ind)) then
    deallocate(k3ind)
    nullify(k3ind)
  end if
  if (associated(mmtexc)) then
    deallocate(mmtexc)
    nullify(mmtexc)
  end if
  if (associated(n3botype)) then
    deallocate(n3botype)
    nullify(n3botype)
  end if
  if (associated(n3bondno)) then
    deallocate(n3bondno)
    nullify(n3bondno)
  end if
  if (associated(n3bondnono)) then
    deallocate(n3bondnono)
    nullify(n3bondnono)
  end if
  if (associated(nthbptr)) then
    deallocate(nthbptr)
    nullify(nthbptr)
  end if
  if (associated(nthrty)) then
    deallocate(nthrty)
    nullify(nthrty)
  end if
  if (associated(ntptyp1)) then
    deallocate(ntptyp1)
    nullify(ntptyp1)
  end if
  if (associated(ntptyp2)) then
    deallocate(ntptyp2)
    nullify(ntptyp2)
  end if
  if (associated(ntptyp3)) then
    deallocate(ntptyp3)
    nullify(ntptyp3)
  end if
  if (associated(ntspec1)) then
    deallocate(ntspec1)
    nullify(ntspec1)
  end if
  if (associated(ntspec2)) then
    deallocate(ntspec2)
    nullify(ntspec2)
  end if
  if (associated(ntspec3)) then
    deallocate(ntspec3)
    nullify(ntspec3)
  end if
  if (associated(lgenerated3)) then
    deallocate(lgenerated3)
    nullify(lgenerated3)
  end if
  if (associated(lthetataper)) then
    deallocate(lthetataper)
    nullify(lthetataper)
  end if
  if (associated(ltdreiding)) then
    deallocate(ltdreiding)
    nullify(ltdreiding)
  end if
  if (associated(ltintra)) then
    deallocate(ltintra)
    nullify(ltintra)
  end if
  if (associated(ltinter)) then
    deallocate(ltinter)
    nullify(ltinter)
  end if
  if (associated(ltib3body)) then
    deallocate(ltib3body)
    nullify(ltib3body)
  end if
  if (associated(ltif3body)) then
    deallocate(ltif3body)
    nullify(ltif3body)
  end if
  if (associated(thbk)) then
    deallocate(thbk)
    nullify(thbk)
  end if
  if (associated(theta)) then
    deallocate(theta)
    nullify(theta)
  end if
  if (associated(thetatapermax)) then
    deallocate(thetatapermax)
    nullify(thetatapermax)
  end if
  if (associated(thetatapermin)) then
    deallocate(thetatapermin)
    nullify(thetatapermin)
  end if
  if (associated(thr1min)) then
    deallocate(thr1min)
    nullify(thr1min)
  end if
  if (associated(thr2min)) then
    deallocate(thr2min)
    nullify(thr2min)
  end if
  if (associated(thr3min)) then
    deallocate(thr3min)
    nullify(thr3min)
  end if
  if (associated(thr1)) then
    deallocate(thr1)
    nullify(thr1)
  end if
  if (associated(thr2)) then
    deallocate(thr2)
    nullify(thr2)
  end if
  if (associated(thr3)) then
    deallocate(thr3)
    nullify(thr3)
  end if
  if (associated(thrho1)) then
    deallocate(thrho1)
    nullify(thrho1)
  end if
  if (associated(thrho2)) then
    deallocate(thrho2)
    nullify(thrho2)
  end if
  if (associated(thrho3)) then
    deallocate(thrho3)
    nullify(thrho3)
  end if
  if (associated(threepoly)) then
    deallocate(threepoly)
    nullify(threepoly)
  end if

  !
  ! module m_ti
  !
  if (associated(dUdlambda)) then
    deallocate(dUdlambda)
    nullify(dUdlambda)
  end if

  !
  ! module transform
  !
  if (associated(tmat)) then
    deallocate(tmat)
    nullify(tmat)
  end if
  if (associated(tmatT)) then
    deallocate(tmatT)
    nullify(tmatT)
  end if
  if (associated(tmatQ)) then
    deallocate(tmatQ)
    nullify(tmatQ)
  end if

  !
  ! module two
  !
  if (associated(symbol2)) then
    deallocate(symbol2)
    nullify(symbol2)
  end if
  if (associated(ipot)) then
    deallocate(ipot)
    nullify(ipot)
  end if
  if (associated(mmexc)) then
    deallocate(mmexc)
    nullify(mmexc)
  end if
  if (associated(mmexcse)) then
    deallocate(mmexcse)
    nullify(mmexcse)
  end if
  if (associated(n2botype)) then
    deallocate(n2botype)
    nullify(n2botype)
  end if
  if (associated(natse)) then
    deallocate(natse)
    nullify(natse)
  end if
  if (associated(ncombipower)) then
    deallocate(ncombipower)
    nullify(ncombipower)
  end if
  if (associated(ntypse)) then
    deallocate(ntypse)
    nullify(ntypse)
  end if
  if (associated(nattab)) then
    deallocate(nattab)
    nullify(nattab)
  end if
  if (associated(ntypab)) then
    deallocate(ntypab)
    nullify(ntypab)
  end if
  if (associated(nptype)) then
    deallocate(nptype)
    nullify(nptype)
  end if
  if (associated(nptyp1)) then
    deallocate(nptyp1)
    nullify(nptyp1)
  end if
  if (associated(nptyp2)) then
    deallocate(nptyp2)
    nullify(nptyp2)
  end if
  if (associated(nspec1)) then
    deallocate(nspec1)
    nullify(nspec1)
  end if
  if (associated(nspec2)) then
    deallocate(nspec2)
    nullify(nspec2)
  end if
  if (associated(nspecptr1)) then
    deallocate(nspecptr1)
    nullify(nspecptr1)
  end if
  if (associated(nspecptr2)) then
    deallocate(nspecptr2)
    nullify(nspecptr2)
  end if
  if (associated(lcombine)) then
    deallocate(lcombine)
    nullify(lcombine)
  end if
  if (associated(lgenerated2)) then
    deallocate(lgenerated2)
    nullify(lgenerated2)
  end if
  if (associated(lintra)) then
    deallocate(lintra)
    nullify(lintra)
  end if
  if (associated(linter)) then
    deallocate(linter)
    nullify(linter)
  end if
  if (associated(leshift)) then
    deallocate(leshift)
    nullify(leshift)
  end if
  if (associated(lgshift)) then
    deallocate(lgshift)
    nullify(lgshift)
  end if
  if (associated(lorder12)) then
    deallocate(lorder12)
    nullify(lorder12)
  end if
  if (associated(lmm3se)) then
    deallocate(lmm3se)
    nullify(lmm3se)
  end if
  if (associated(ltib2body)) then
    deallocate(ltib2body)
    nullify(ltib2body)
  end if
  if (associated(ltif2body)) then
    deallocate(ltif2body)
    nullify(ltif2body)
  end if
  if (associated(atoma)) then
    deallocate(atoma)
    nullify(atoma)
  end if
  if (associated(atomb)) then
    deallocate(atomb)
    nullify(atomb)
  end if
  if (associated(twopot)) then
    deallocate(twopot)
    nullify(twopot)
  end if
  if (associated(epsilon)) then
    deallocate(epsilon)
    nullify(epsilon)
  end if
  if (associated(eshift)) then
    deallocate(eshift)
    nullify(eshift)
  end if
  if (associated(gshift)) then
    deallocate(gshift)
    nullify(gshift)
  end if
  if (associated(repcut)) then
    deallocate(repcut)
    nullify(repcut)
  end if
  if (associated(rhopot)) then
    deallocate(rhopot)
    nullify(rhopot)
  end if
  if (associated(rpot)) then
    deallocate(rpot)
    nullify(rpot)
  end if
  if (associated(rpot2)) then
    deallocate(rpot2)
    nullify(rpot2)
  end if
  if (associated(scale14)) then
    deallocate(scale14)
    nullify(scale14)
  end if
  if (associated(sigma)) then
    deallocate(sigma)
    nullify(sigma)
  end if
  if (associated(tpot)) then
    deallocate(tpot)
    nullify(tpot)
  end if
  if (associated(tapergrad)) then
    deallocate(tapergrad)
    nullify(tapergrad)
  end if
  if (associated(taperpot)) then
    deallocate(taperpot)
    nullify(taperpot)
  end if

  !
  ! module freeze
  !
  if (associated(iufree)) then
    deallocate(iufree)
    nullify(iufree)
  end if
  if (associated(lufree)) then
    deallocate(lufree)
    nullify(lufree)
  end if
  if (associated(rufree)) then
    deallocate(rufree)
    nullify(rufree)
  end if
  if (associated(xufree)) then
    deallocate(xufree)
    nullify(xufree)
  end if

  !
  ! module uffdata
  !
  if (associated(symbolUFFspec)) then
    deallocate(symbolUFFspec)
    nullify(symbolUFFspec)
  end if
  if (associated(natUFFspec)) then
    deallocate(natUFFspec)
    nullify(natUFFspec)
  end if
  if (associated(ntypUFFspec)) then
    deallocate(ntypUFFspec)
    nullify(ntypUFFspec)
  end if
  if (associated(nUFFtype)) then
    deallocate(nUFFtype)
    nullify(nUFFtype)
  end if
  if (associated(UFFr)) then
    deallocate(UFFr)
    nullify(UFFr)
  end if
  if (associated(UFFtheta)) then
    deallocate(UFFtheta)
    nullify(UFFtheta)
  end if
  if (associated(UFFtor)) then
    deallocate(UFFtor)
    nullify(UFFtor)
  end if
  if (associated(UFFx)) then
    deallocate(UFFx)
    nullify(UFFx)
  end if
  if (associated(UFFd)) then
    deallocate(UFFd)
    nullify(UFFd)
  end if
  if (associated(UFFzeta)) then
    deallocate(UFFzeta)
    nullify(UFFzeta)
  end if
  if (associated(UFFZeff)) then
    deallocate(UFFZeff)
    nullify(UFFZeff)
  end if
  if (associated(UFFKoop)) then
    deallocate(UFFKoop)
    nullify(UFFKoop)
  end if
  if (associated(UFFthetaoop)) then
    deallocate(UFFthetaoop)
    nullify(UFFthetaoop)
  end if
  if (associated(UFFchi)) then
    deallocate(UFFchi)
    nullify(UFFchi)
  end if

  !
  ! module vectors
  ! 
  ! use of 'public'
  ! deallocation of following member variables are not touched; 06.2024 update


  !
  ! module velocities
  !
  if (associated(velx)) then
    deallocate(velx)
    nullify(velx)
  end if
  if (associated(vely)) then
    deallocate(vely)
    nullify(vely)
  end if
  if (associated(velz)) then
    deallocate(velz)
    nullify(velz)
  end if
  if (associated(x2)) then
    deallocate(x2)
    nullify(x2)
  end if
  if (associated(y2)) then
    deallocate(y2)
    nullify(y2)
  end if
  if (associated(z2)) then
    deallocate(z2)
    nullify(z2)
  end if
  if (associated(x3)) then
    deallocate(x3)
    nullify(x3)
  end if
  if (associated(y3)) then
    deallocate(y3)
    nullify(y3)
  end if
  if (associated(z3)) then
    deallocate(z3)
    nullify(z3)
  end if
  if (associated(x4)) then
    deallocate(x4)
    nullify(x4)
  end if
  if (associated(y4)) then
    deallocate(y4)
    nullify(y4)
  end if
  if (associated(z4)) then
    deallocate(z4)
    nullify(z4)
  end if
  if (associated(x5)) then
    deallocate(x5)
    nullify(x5)
  end if
  if (associated(y5)) then
    deallocate(y5)
    nullify(y5)
  end if
  if (associated(z5)) then
    deallocate(z5)
    nullify(z5)
  end if

  !
  ! module xcgc
  !
  if (associated(gc)) then
    deallocate(gc)
    nullify(gc)
  end if
  if (associated(xc)) then
    deallocate(xc)
    nullify(xc)
  end if
  if (associated(xcother)) then
    deallocate(xcother)
    nullify(xcother)
  end if

  !
  ! module what
  !

  ! --------------------------------------------------------------------------------------------
  ! end modules.F90
  ! --------------------------------------------------------------------------------------------
  !


  !
  ! module m_gfnff_nbr3
  !
  if (associated(n3atomptr)) then
    deallocate(n3atomptr)
    nullify(n3atomptr)
  end if
  if (associated(n3atomrptr)) then
    deallocate(n3atomrptr)
    nullify(n3atomrptr)
  end if
  if (associated(n3nbr)) then
    deallocate(n3nbr)
    nullify(n3nbr)
  end if
  if (associated(n3nbrshell)) then
    deallocate(n3nbrshell)
    nullify(n3nbrshell)
  end if
  if (associated(x3nbr)) then
    deallocate(x3nbr)
    nullify(x3nbr)
  end if
  if (associated(y3nbr)) then
    deallocate(y3nbr)
    nullify(y3nbr)
  end if
  if (associated(z3nbr)) then
    deallocate(z3nbr)
    nullify(z3nbr)
  end if
  if (associated(nb3list)) then
    deallocate(nb3list)
    nullify(nb3list)
  end if
  if (associated(rilb3)) then
    deallocate(rilb3)
    nullify(rilb3)
  end if


  !
  ! module kim_models
  !
  if (associated(kim_ncontributing_only)) then
    deallocate(kim_ncontributing_only)
    nullify(kim_ncontributing_only)
  end if
  if (associated(kim_neighbourlist)) then
    deallocate(kim_neighbourlist)
    nullify(kim_neighbourlist)
  end if
  if (associated(kim_nspec)) then
    deallocate(kim_nspec)
    nullify(kim_nspec)
  end if
  if (associated(kim_nspecies)) then
    deallocate(kim_nspecies)
    nullify(kim_nspecies)
  end if
  if (associated(kim_ncontributing)) then
    deallocate(kim_ncontributing)
    nullify(kim_ncontributing)
  end if
  if (associated(kim_coord)) then
    deallocate(kim_coord)
    nullify(kim_coord)
  end if
  if (associated(kim_forces)) then
    deallocate(kim_forces)
    nullify(kim_forces)
  end if
  if (associated(kim_particle)) then
    deallocate(kim_particle)
    nullify(kim_particle)
  end if
  if (associated(kim_cutoffs)) then
    deallocate(kim_cutoffs)
    nullify(kim_cutoffs)
  end if
  if (associated(lkim_model_cfg_OK)) then
    deallocate(lkim_model_cfg_OK)
    nullify(lkim_model_cfg_OK)
  end if

  !
  ! module m_pr
  !
  if (associated(pekin)) then
    deallocate(pekin)
    nullify(pekin)
  end if
  if (associated(taubcfg)) then
    deallocate(taubcfg)
    nullify(taubcfg)
  end if
  if (associated(tautcfg)) then
    deallocate(tautcfg)
    nullify(tautcfg)
  end if
  if (associated(pr_conscfg)) then
    deallocate(pr_conscfg)
    nullify(pr_conscfg)
  end if

  !
  ! module m_pdfneutron : pointer
  !
  if (associated(pdffiles)) then
    deallocate(pdffiles)
    nullify(pdffiles)
  end if
  if (associated(rmaxcfgs)) then
    deallocate(rmaxcfgs)
    nullify(rmaxcfgs)
  end if
  if (associated(wmaxcfgs)) then
    deallocate(wmaxcfgs)
    nullify(wmaxcfgs)
  end if
  if (associated(wmincfgs)) then
    deallocate(wmincfgs)
    nullify(wmincfgs)
  end if
  if (associated(nrbinscfgs)) then
    deallocate(nrbinscfgs)
    nullify(nrbinscfgs)
  end if
  if (associated(nkpointscfg)) then
    deallocate(nkpointscfg)
    nullify(nkpointscfg)
  end if
  if (associated(lshrinkset)) then
    deallocate(lshrinkset)
    nullify(lshrinkset)
  end if
  if (associated(ldispersionset)) then
    deallocate(ldispersionset)
    nullify(ldispersionset)
  end if
  if (associated(lunitrad)) then
    deallocate(lunitrad)
    nullify(lunitrad)
  end if
  if (associated(lunitthz)) then
    deallocate(lunitthz)
    nullify(lunitthz)
  end if
  if (associated(lunitmev)) then
    deallocate(lunitmev)
    nullify(lunitmev)
  end if
  if (associated(lunitcm)) then
    deallocate(lunitcm)
    nullify(lunitcm)
  end if
  if (associated(unitsnamecfgs)) then
    deallocate(unitsnamecfgs)
    nullify(unitsnamecfgs)
  end if

  !
  ! module what
  !

  return

end subroutine gulpklmc_deallocate_all
