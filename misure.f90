!------------------------------------------------------------------------------
!
! MODULE: misure
!
!> @author
!> Federico Zadra
!
! DESCRIPTION:
!> raccolta delle routine di calcolo
!
!------------------------------------------------------------------------------
MODULE misure

USE latticegen
USE inoutput

CONTAINS
!------------------------------------------------------------------------------
! DESCRIPTION:
!> Magnetizzazione media
!> @brief
!> Calcola la magnetizzazione media dato il reticolo
!
!> @param[in] latt reticolo
!> @return M magnetizzazione media
!------------------------------------------------------------------------------
   FUNCTION M(latt)
      REAL(DP) :: M
      INTEGER, DIMENSION (:,:) :: latt

      M=SUM(latt)

   END FUNCTION M

!------------------------------------------------------------------------------
! DESCRIPTION:
!> Energia del sito
!> @brief
!> Calcola l'energia del singolo sito appartenete al reticolo in relazione
!> ai primi vicini
!> @param[in] s spin di riferimento
!> @param[in] pv  vettore dei primi vicini
!> @param[in] d numero dei primi vicini
!> @return Energia del reticolo
!---------------------------------------------------------------------------
   INTEGER FUNCTION E(s,pv,d)

    IMPLICIT NONE

        INTEGER :: s,d
        INTEGER, DIMENSION(:) :: pv

        INTEGER :: i

        E=0

        DO i=1,d
           E=E-s*pv(i)
        END DO

    END FUNCTION E

!------------------------------------------------------------------------------
! DESCRIPTION:
!> Energia totale
!> @brief
!> Calcola l'energia totale del reticolo
!
!> @param[in] no numero colonne
!> @param[in] nv numero righe
!> @param[in] latt1 reticolo
!> @param[in] pv vettore dei primi vicini
!> @param[in] d numero dei primi vicini
!> @return et energia totale del reticolo
!---------------------------------------------------------------------------
    FUNCTION et(no,nv,latt1,pv,d)

        INTEGER :: et
        INTEGER :: no,nv,d

        INTEGER, DIMENSION(:,:) :: latt1
        INTEGER, DIMENSION (:) :: pv
        INTEGER, DIMENSION(2,d) :: xy

        INTEGER :: x,y

        et=0

        DO x=1,no
            DO y=1,nv
                xy=adiacenti(x,y,no,nv,d)
                DO i=1,d
                    pv(i)=latt1(xy(1,i),xy(2,i))
                END DO
                et=et+E(latt1(x,y),pv,d)
            END DO
        END DO

    END FUNCTION et

   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !> Cambio energia del sito
   !> @brief
   !> Valutazione dell'energia del sito con spin invertito calcolando l'interazione con i primi vicini
   !> @param[in] latt1 reticolo
   !> @param[in] no numero colonne
   !> @param[in] nv numero righe
   !> @param[in] d numero dei primi vicini
   !> @param[in] t temperatura
   !---------------------------------------------------------------------------
   SUBROUTINE cambia(latt1,no,nv,d,t)

       IMPLICIT NONE

       INTEGER, DIMENSION (:,:) :: latt1
       INTEGER :: no, nv
       INTEGER :: d
       REAL(DP) :: t

       INTEGER :: s,x, y,ij
       INTEGER, DIMENSION (d) :: pv
       INTEGER, DIMENSION (2,d) :: xy
       INTEGER :: es
       REAL(DP) ::  a  , p

       x=cascoord(no)
       y=cascoord(nv)
       s=latt1(x,y)

       xy=adiacenti(x,y,no,nv,d)

       DO ij=1,d
          pv(ij)=latt1(xy(1,ij),xy(2,ij))
       END DO

       es=-(E(s,pv,d))
       IF (es<0) THEN
          latt1(x,y)=-latt1(x,y)
       ELSE
          CALL RANDOM_NUMBER(a)
          p=EXP(-2*es/(t))
          IF (p>a) THEN
             latt1(x,y)=-latt1(x,y)
          END IF
       END IF

    END SUBROUTINE cambia

END MODULE


