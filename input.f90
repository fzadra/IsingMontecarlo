!------------------------------------------------------------------------------
!
! MODULE: inoutput
!
!> @author
!> Federico Zadra
!
! DESCRIPTION:
!> raccolta delle routine di input/output.
!------------------------------------------------------------------------------
MODULE inoutput

INTEGER, PARAMETER :: DP = selected_real_kind(14,200)

CONTAINS
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !> input parametri
   !> @brief
   !> Routine di input dei dati per la simulazione
   !
   !> @param[out] no  numero righe
   !> @param[out] nv  numero colonne
   !> @param[out] d   numero dei primi vicini
   !---------------------------------------------------------------------------
    SUBROUTINE input(no,nv,d)

        IMPLICIT NONE
        INTEGER, INTENT(OUT) :: no, nv, d
        INTEGER :: retyp

        retyp=2

        PRINT*, "Tipo di Reticolo (1=quadrato, 2=triangolare, 3=esagonale): "
        READ(*,*) retyp
        PRINT*, "Numero di Siti Orizzontali (no): "
        READ(*,*) no
        PRINT*, "Numero di Siti Verticali (nv): "
        READ(*,*) nv

        IF (retyp==1) THEN       ! Caso Quadrato
            d=4
         ELSE IF (retyp==2) THEN ! Caso Triangolare
            d=6
            IF (MODULO(nv,2)==1) THEN
                nv=nv+1
            END IF
         ELSE IF (retyp==3) THEN ! Caso Esagonale
            d=3
            IF (MODULO(no,2)==1) THEN
                no=no+1
            END IF
            IF (MODULO(nv,2)==1) THEN
                nv=nv+1
            END IF
        END IF

     END SUBROUTINE input

   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !> Output dati
   !> @brief
   !> uscita dati su file
   !
   !> @param[in] t    temperatura
   !> @param[in] mag  magnetizzazione
   !> @param[in] en   energia totale
   !> @param[in] x1   Suscettivita'
   !> @param[in] c1   capacita' termica
   !> @param[in] eq   energia quadratica
   !> @param[in] enn  energia per sito
   !---------------------------------------------------------------------------
   SUBROUTINE output(t,mag,en,x1,c1,eq,enn)

        REAL(DP), INTENT(IN) :: t, mag, en, x1, c1, eq, enn
        INTEGER :: error

        WRITE(UNIT=11,FMT='(F5.2,F15.10,F18.10,F15.10,F15.10,F20.10,F15.10)',IOSTAT=error) t, mag, en, x1, c1, eq, enn
        IF (error/=0) THEN
            CALL ERRORE('output')
        END IF

    END SUBROUTINE output

   !---------------------------------------------------------------------------
   !
   ! DESCRIPTION:
   !> visualizzazione errore
   !> @brief
   !> Routine di visualizzazione dell'eventuale errore
   !
   !> @param[in] errloc nome routine chiamante
   !---------------------------------------------------------------------------
   SUBROUTINE errore(errloc)

        CHARACTER(LEN=*) :: errloc
        PRINT*, "ERRORE IN ", errloc
        STOP

    END SUBROUTINE errore

    !---------------------------------------------------------------------------
    !
    ! DESCRIPTION:
    !> Conversione intero stringa
    !> @brief
    !> Converte un intero nella sua rappresentazione in caratteri
    !> @param[in] i numero intero
    !> @return res numero sotto forma di carattere
    !---------------------------------------------------------------------------
    !
    FUNCTION itoa(i) RESULT(res)

        INTEGER, INTENT(IN) :: i

        CHARACTER(:), ALLOCATABLE :: res

        CHARACTER(RANGE(i)+2) :: tmp

        WRITE(tmp,'(i0)') i
        res = TRIM(tmp)

    END FUNCTION

    !---------------------------------------------------------------------------
    !
    ! DESCRIPTION:
    !> Dati per GnuPlot
    !> @brief
    !> La subroutine scrive un file interpretabile da GnuPlot per rappresentare
    !! graficamente la configurazione campione fornita dal reticolo in entrata latt1.
    !>
    !> @param[in] latt1 reticolo
    !> @param[in] t temperatura
    !> @param[in] no colonne del reticolo
    !> @param[in] nv righe del reticolo
    !---------------------------------------------------------------------------
    !
    SUBROUTINE graf(latt1,t,no,nv)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: no,nv
        INTEGER, INTENT(IN), DIMENSION(no,nv) :: latt1
        REAL(DP), INTENT(IN) :: t

        CHARACTER(LEN=3) :: lt,lx,ly,lx1,ly1
        INTEGER :: error, x,y

        lt=itoa(INT(t))//"."//itoa(MOD(INT(t*10),10))

        OPEN(Unit=1,file="grafico_"//lt,iostat=error,status='new',action='READWRITE')
        IF (error/=0) CALL errore('opengraf')
        WRITE(Unit=1,FMT='(a)',IOSTAT=error) "set term png"
        lx=itoa(no)
        ly=itoa(nv)
            WRITE(Unit=1,FMT='(a,a,a,a,a)',IOSTAT=error) "set output 'T=", lt,"_", lx,".png'"
            WRITE(Unit=1,FMT='(a,a,a)',IOSTAT=error) "set title 'Ising Configuration T=", lt, "'"
            WRITE(Unit=1,FMT='(a)',IOSTAT=error) "unset key"
            WRITE(Unit=1,FMT='(a)',IOSTAT=error) "unset tics"
            WRITE(Unit=1,FMT='(a,a,a)',IOSTAT=error) "set xrange [0 : ", lx, "]"
            WRITE(Unit=1,FMT='(a,a,a)',IOSTAT=error) "set yrange [0 : ", ly, "]"
            WRITE(Unit=1,FMT='(a)',IOSTAT=error)
            DO x=1,no
                DO y=1,nv
                lx=itoa(x)
                ly=itoa(y)
                lx1=itoa(x-1)
                ly1=itoa(y-1)
                !
                IF (latt1(x,y)==1) THEN
                    WRITE(Unit=1,FMT='(a,a,a,a,a,a,a,a,a)',IOSTAT=error) "set object rectangle from ", lx1,",", ly1,&
                    " to ",lx,",",ly," fc rgb 'red'"
                ELSE
                    WRITE(Unit=1,FMT='(a,a,a,a,a,a,a,a,a)',IOSTAT=error) "set object rectangle from ", lx1,",", ly1,&
                    " to ",lx,",",ly," fc rgb 'blue'"
                END IF
                END DO
            END DO
            WRITE(Unit=1,FMT='(a)',IOSTAT=error) "plot 1"
            WRITE(Unit=1,FMT='(a)',IOSTAT=error) "quit"
            CLOSE(UNIT=1,IOSTAT=error,STATUS='keep')
            IF (error/=0) CALL errore('closegraf')
     END SUBROUTINE graf

    !---------------------------------------------------------------------------
    !
    ! DESCRIPTION:
    !> Generazione nome file
    !> @brief
    !> Genera il nome del file dati in uscita dai parametri forniti
    !> @param[in] no colonne del reticolo
    !> @param[in] nv righe del reticolo
    !> @param[in] d vicini
    !> @return filename nome del file dati
    !---------------------------------------------------------------------------
    FUNCTION filename(no,nv,d)

        INTEGER, INTENT(IN) :: no,nv,d  !retyp

        CHARACTER(LEN=128) :: filename

        filename="output."//itoa(no)//"x"//itoa(nv)//"."
        IF (d==4) then
            filename=trim(filename)//"quad.dat"
        ELSEIF (d==6) then
           filename=trim(filename)//"tri.dat"
        ELSE  !(d==3)
           filename=trim(filename)//"esa.dat"
        END IF


     END FUNCTION filename

END MODULE

