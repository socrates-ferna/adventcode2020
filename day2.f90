PROGRAM day2
    IMPLICIT NONE
    INTEGER :: i, status, ubound, lbound, pos, count, ok, pass_end, ok2, lpos, upos
    CHARACTER(100) :: msg
    CHARACTER(200) :: line
    CHARACTER(1) :: rule


    ok = 0
    ok2 = 0
    lbound = 0
    ubound = 0


    OPEN(UNIT=100,FILE='day2input.txt', STATUS='OLD', ACTION='READ', IOSTAT=status, IOMSG=msg)

    DO
        count = 0
        pos = 1
        READ(100,'(A200)',IOSTAT=status,IOMSG=msg) line
        !PRINT*, 'I read: ',line

        IF(status < 0) EXIT

        IF( line(2:2) .eq. '-' ) THEN
            READ(line(1:1),'(I1)') lbound
            !PRINT*, 'Lower bound is: ', lbound
            pos = 3

            IF(line(pos+1:pos+1) == ' ') THEN
                READ(line(pos:pos),'(I1)') ubound 
                pos = 5
            ELSE
                READ(line(pos:pos+1),'(I2)') ubound
                pos = 6
            END IF

        ELSE
            READ(line(1:2),'(I2)') lbound
            !PRINT*, 'Lower bound is: ', lbound
            pos = 4
            READ(line(pos:pos+2),'(I2)') ubound
            pos = 7
        END IF
        PRINT*, 'Bounds for line ', line
        PRINT*, 'lbound=',lbound,'ubound',ubound
        rule = line(pos:pos)
        PRINT*, 'rule is: ', rule
        pos = pos + 3
        !check password now
        pass_end = index(line(pos:200),' ') + pos - 2
        PRINT*,'pass end is: ', pass_end
        DO i = pos, pass_end  !! LOOP FOR PART ONE
            IF (line(i:i) == rule) THEN 
                count = count + 1
            END IF
        END DO

        !!! PART TWO CONDITION IS THE lbound-ith or the ubound-ith must coincide with the rule 
        !!! BUT  NOT BOTH!!! If both coincide then it's not valid
        !!! if not fulfiled then the pass is not valid
        lpos = pos + lbound - 1
        upos = pos + ubound - 1

        IF( line(lpos:lpos) == rule  .and.  line(upos:upos) == rule ) THEN
            GOTO 1
        ELSE IF( line(lpos:lpos) == rule  .or.  line(upos:upos) == rule ) THEN
            ok2 = ok2 + 1
            PRINT*, 'FOUND SECOND PART OK'
        END IF

        1 CONTINUE
        IF(count >= lbound .and. count <= ubound) THEN
            ok = ok + 1
            PRINT*, 'Found an ok, total count= ', ok
        END IF

    END DO
    PRINT*, 'Final count: ', ok
    PRINT*, 'Final count second part: ', ok2
    CLOSE(100)

END PROGRAM day2