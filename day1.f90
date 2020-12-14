PROGRAM find2020
    IMPLICIT NONE
    INTEGER :: status, i, j, k, nrows, product
    INTEGER, ALLOCATABLE, DIMENSION(:) :: input
    INTEGER, DIMENSION(10000) :: aux
    CHARACTER(300) :: msg
    PRINT*,'---------------------------'
    PRINT*,'!        PART ONE         !'
    PRINT*,'---------------------------'
    product = 0
    OPEN(UNIT=100, FILE='input1.txt', ACTION='READ', STATUS='OLD', IOSTAT=status,IOMSG=msg)
    
    j = 1
    nrows = 0
    DO
        READ(100,'(I5)', IOSTAT=status, IOMSG=msg) aux(j)
        nrows = nrows + 1

        IF(nrows >= 10000) THEN
            PRINT*, 'Input limit reached, closing file...'
            EXIT
        END IF

        IF(status < 0) EXIT

        j = j + 1
    END DO
    CLOSE(100)

    nrows = nrows - 1 ! Discard EOF character
    ALLOCATE(input(nrows));input(1:nrows) = aux(1:nrows)

    outer: DO i = 1, nrows
        inner: DO k = 1, nrows
            IF(k <= i) CYCLE inner ! Discard already computed sums
            IF(input(i) + input(k) == 2020) product = input(i)*input(k)
        END DO inner
    END DO outer

    IF(product == 0) THEN
        PRINT*, "DIDN'T FIND A RESULT'"
    ELSE
        PRINT*, 'Result first part is:', product
    END IF

    PRINT*,'---------------------------'
    PRINT*,'!        PART TWO         !'
    PRINT*,'---------------------------'
    product = 0
    outer2: DO i = 1, nrows
        DO j = 1, nrows
            DO k = 1, nrows
                IF(input(i) + input(j) + input(k) == 2020) THEN
                    product = input(i) * input(j) * input(k)
                    EXIT outer2
                END IF
            END DO
        END DO
    END DO outer2

    IF(product == 0) THEN
        PRINT*,'No solution was found'
    ELSE
        PRINT*,'Product result is: ',product
    END IF

END PROGRAM find2020