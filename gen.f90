!------------------------------------------------------------------------------
!
! MODULE: latticegen
!
!> @author
!> Federico Zadra
!
! DESCRIPTION:
!> Raccolta delle routine relative al reticolo
!------------------------------------------------------------------------------
MODULE latticegen

USE inoutput

CONTAINS
   !---------------------------------------------------------------------------
   !
   ! DESCRIPTION:
   !> Randomizzazione iniziale del reticolo
   !
   !> @param[out] latt matrice dei spin
   !> @param[in] no numero colonne
   !> @param[in] nv numero righe
   !---------------------------------------------------------------------------
   SUBROUTINE randomreticle(latt,no,nv)

        IMPLICIT NONE
        INTEGER, DIMENSION(no,nv),INTENT(OUT) :: latt  !> @var latt matrice dei spin
        INTEGER, INTENT(IN) :: no,nv

        INTEGER :: i,j
        REAL(DP) :: rand

        latt=-1
        DO i=1,no
            DO j=1,nv
                CALL RANDOM_NUMBER(rand)
                IF (rand>0.5) THEN
                    latt(i,j)=1
                ELSE
                    latt(i,j)=-1
                END IF
            END DO
        END DO

    END SUBROUTINE randomreticle

   !---------------------------------------------------------------------------
   !
   ! DESCRIPTION:
   !> funzione modulo modificata
   !> @brief Il resto della divisione intera va' da 1 a n e non da 0 a n-1
   !>
   !> @param[in] i dividendo
   !> @param[in] j divisore
   !> @return modula resto
   !---------------------------------------------------------------------------
   FUNCTION modula(i,j) RESULT(resto)

        IMPLICIT NONE

        INTEGER :: i,j
        INTEGER :: resto

        resto=MOD(i,j)

        IF (resto==0) THEN
            resto=j
        END IF

    END FUNCTION

    !---------------------------------------------------------------------------
    !
    ! DESCRIPTION:
    !> vettore primi vicini
    !> @brief
    !> Localizza i primi vicini di ogni sito tenendo in considerazione anche
    !> le condizioni al contorno periodiche.
    !> @param[in] x coordinata x del sito
    !> @param[in] y coordinata y del sito
    !> @param[in] no numero colonne
    !> @param[in] nv numero righe
    !> @param[in] d numero dei primi vicini
    !> @return M magnetizzazione media
    !---------------------------------------------------------------------------
    FUNCTION adiacenti(x,y,no,nv,d) RESULT(xypv)

        INTEGER, INTENT(IN) :: x,y
        INTEGER, INTENT(IN) :: no,nv
        INTEGER, INTENT(IN) :: d
        INTEGER, DIMENSION(2,d) :: xypv

        xypv=0
        IF (d==4) THEN
            xypv(:,1)=[modula(x-1,no),y]
            xypv(:,2)=[modula(x+1,no),y]
            xypv(:,3)=[x,modula(y-1,nv)]
            xypv(:,4)=[x,modula(y+1,nv)]
        ELSE IF (d==6) THEN
            xypv(:,1)=[modula(x-1,no),y]
            xypv(:,2)=[modula(x+1,no),y]
            xypv(:,3)=[x,modula(y-1,nv)]
            xypv(:,4)=[x,modula(y+1,nv)]
            IF (modula(y,2)==1) THEN
                xypv(:,5)=[modula(x-1,no),modula(y-1,nv)]
                xypv(:,6)=[modula(x-1,no),modula(y+1,nv)]
            ELSE
                xypv(:,5)=[modula(x+1,no),modula(y-1,nv)]
                xypv(:,6)=[modula(x+1,no),modula(y+1,nv)]
            ENDIF
        ELSE              ! IF (d==3) THEN
            xypv(:,1)=[x,modula(y-1,nv)]
            xypv(:,2)=[x,modula(y+1,nv)]
            IF (modula(y,2)==modula(x,2)) THEN
                xypv(:,3)=[modula(x+1,no),y]
            ELSE
                xypv(:,3)=[modula(x-1,no),y]
            END IF
        END IF

    END FUNCTION

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    !> Coordinata random
    !> @brief Genera una coordinata di valore casuale compresa nella dimensione indicata
    !> @param[in] lunghezza valore max della coordinata
    !> @return coord coordinata di valore casuale
    !---------------------------------------------------------------------------
    FUNCTION cascoord(lunghezza) RESULT(coord)

        INTEGER, INTENT(IN) :: lunghezza
        INTEGER :: coord
        REAL :: xcoord

        CALL RANDOM_NUMBER(xcoord)
        coord=INT(xcoord*(lunghezza+1))+1
        IF (coord>lunghezza) coord=lunghezza

    END FUNCTION  cascoord

END MODULE
