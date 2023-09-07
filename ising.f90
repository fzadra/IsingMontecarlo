!------------------------------------------------------------------------------
!
! MODULE: ising
!
!> @author
!> Federico Zadra
!
! DESCRIPTION:
!> Modulo principale del programma di simulazione.
!------------------------------------------------------------------------------
MODULE principale

    USE inoutput
    USE latticegen
    USE misure

CONTAINS

   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !> Routine principale del programma
   !> @brief
   !> Ciclo principale del programma di simulazione
   !---------------------------------------------------------------------------

SUBROUTINE ising()

    IMPLICIT NONE

    INTEGER, DIMENSION(:,:), ALLOCATABLE :: latt1
    INTEGER :: no
    INTEGER :: nv
    INTEGER ::  d

    ! Variabili delle Misure
    REAL(DP) :: mag, magq, etot, e0, eq

    ! Variabili Termodinamiche di sistema
    REAL(DP) :: t, temp1

    ! Ciclo Temperatura
    REAL(DP) :: dt
    INTEGER :: tspace, temp

    ! Variabili per Monte Carlo
    INTEGER, DIMENSION(:), ALLOCATABLE :: pv
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: xy

    ! Variabili per Metropolis
    INTEGER :: nstep
    INTEGER :: o

    ! variabili interne
    INTEGER :: i
    INTEGER :: error

    ! Inizializzazione variabili di sistema
    mag=0.
    e0=0
    eq=0
    nstep=10000

    CALL input(no,nv,d)

    ! Reticolo di partenza

    ! Allocazione dei vettori
    ALLOCATE(pv(d))
    ALLOCATE(xy(2,d))
    ALLOCATE(latt1(no,nv))

    ! Randomizza il reticolo di partenza
    CALL randomreticle(latt1,no,nv)

    ! Apre il file per l'output dei dati
    OPEN(UNIT=11,FILE=trim(filename(no,nv,d)),IOSTAT=error,STATUS='REPLACE',ACTION='READWRITE')
    IF (error/=0) CALL ERRORE('open')

    dt=0.1         ! Passo della temperatura
    t=5            ! Temperatura massima

    tspace=INT((t-0.5)/dt)

    temperatura: DO temp=0, tspace

        DO i=1, 1000      ! Ciclo Transiente
            CALL cambia(latt1,no,nv,d,t)  ! selezione casuale dello spin
        END DO

        montecarlo: DO i=1, nstep

            metropolis: DO o=1, no*nv

                CALL cambia(latt1,no,nv,d,t) ! selezione casuale dello spin

            END DO metropolis

            ! Sistemazione degli Osservabili

            etot=(REAL(et(no,nv,latt1,pv,d)))/2.0

            temp1=m(latt1)
            mag=mag+ABS(temp1)
            magq=magq+(temp1*temp1)
            e0=e0+etot
            eq=eq+(etot*etot)

        END DO montecarlo

        mag=mag/nstep
        e0=e0/nstep
        eq=eq/nstep
        magq=magq/nstep

        CALL output(t,mag/(no*nv),e0,((magq)-(mag*mag))/(no*nv*t),&
                    ((eq)-(e0*e0))/(no*nv*t*t),eq, e0/(no*nv))

        magq=0.0
        mag=.0
        eq=0
        e0=0

        ! subroutine opzionale per la rappresentazione
        ! grafica del reticolo

        CALL graf(latt1,t,no,nv)

        t=t-dt

    END DO temperatura

    ! operazioni di chiusura

    DEALLOCATE(latt1)
    CLOSE(UNIT=11,IOSTAT=error,STATUS='KEEP')
    IF (error/=0) CALL ERRORE('close')

END SUBROUTINE ising

END MODULE principale






