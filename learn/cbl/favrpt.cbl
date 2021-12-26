       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
       AUTHOR. ED ACKERMAN.
       DATE-WRITTEN. 01/01/2021
       DATE-COMPILED. 
       INSTALLATION. MORONS, LOSERS, AND BIMBOS.
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-3081.
       OBJECT-COMPUTER. IBM-3081.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAV-GRPS  ASSIGN TO FAVIN.
           SELECT FAV-RPT ASSIGN TO PRTLINE.
      *---------------------------------------------------------------*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*
       FD  FAV-GRPS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAV-GRP-REC.
      *---------------------------------------------------------------*
       01  FAV-GRP-REC.
           05  ARTIST-NAME             PIC X(30).
           05  NUMBER-MUSICIANS        PIC 9(02).
           05  MUSICAL-GENRE           PIC X(12).
           05  CD-COST                 PIC 9(03)V99.
           05  SHIPPING-COST           PIC 9(02)V99.
           05  TAX                     PIC 9(02)V99.
           05  BAND-IS-STILL-TOGETHER  PIC X(01).
      *---------------------------------------------------------------*
       FD  FAV-RPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PRINT-RECORD.
      *---------------------------------------------------------------*
       01  PRINT-RECORD.
      *    05  CARRAGE-CONTROL             PIC X(01).
           05  PRINT-LINE                  PIC X(132).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01  PRINT-LINES.
           05  NEXT-REPORT-LINE            PIC X(132) VALUE SPACE.
      *---------------------------------------------------------------*
           05  WS-PRINT-RECORD.
               10  WS-ARTIST-NAME          PIC X(30).
               10  FILLER                  PIC X(02) VALUE SPACE.
               10  WS-NUMBER-MUSICIANS     PIC 9(02).
               10  FILLER                  PIC X(03) VALUE SPACE.
               10  WS-MUSICAL-GENRE        PIC X(12).
               10  FILLER                  PIC X(02) VALUE SPACE.
               10  WS-CD-COST              PIC $$$9.99.
               10  FILLER                  PIC X(02) VALUE SPACE.
               10  WS-SHIPPING-COST        PIC $$9.99.
               10  FILLER                  PIC X(02) VALUE SPACE.
               10  WS-TAX                  PIC $$9.99.
               10  FILLER                  PIC X(02) VALUE SPACE.
               10  WS-TOTAL-COST           PIC $,$$9.99.
               10  FILLER                  PIC X(02) VALUE SPACE.
               10  WS-BAND-STILL-TOGETHER  PIC X(01).
      *---------------------------------------------------------------*
          05  HEADING-LINE-1.
               10  FILLER      PIC X(06) VALUE 'DATE: '.
               10  HDR-DAY     PIC X(02).
               10  FILLER      PIC X(01) VALUE '/'.
               10  HDR-MO      PIC X(02).
               10  FILLER      PIC X(01) VALUE '/'.
               10  HDR-YR      PIC X(04).
               10  FILLER      PIC X(28) VALUE SPACES.
               10  FILLER      PIC X(14) VALUE 'FAVORITE BANDS'.
               10  FILLER      PIC X(28) VALUE SPACES.
               10  FILLER      PIC X(10) VALUE 'PAGE NUM: '.
               10  H1-PAGE-NUM PIC 999.
      *---------------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER  PIC X(20) VALUE 'ARTIST              '.
               10  FILLER  PIC X(20) VALUE '            NUM  MUS'.
               10  FILLER  PIC X(20) VALUE 'IC         CD       '.
               10  FILLER  PIC X(20) VALUE 'TAX     SHIP     TOT'.
               10  FILLER  PIC X(20) VALUE 'AL    STILL         '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(12) VALUE '            '.
      *---------------------------------------------------------------*
            05  HEADING-LINE-3.
               10  FILLER  PIC X(20) VALUE 'NAME                '.
               10  FILLER  PIC X(20) VALUE '            MUS  GEN'.
               10  FILLER  PIC X(20) VALUE 'RE         COST     '.
               10  FILLER  PIC X(20) VALUE 'AMT     COST     COS'.
               10  FILLER  PIC X(20) VALUE 'T     TOGETHER      '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(12) VALUE '            '.
      *---------------------------------------------------------------*
            05  HEADING-LINE-4.
               10  FILLER  PIC X(20) VALUE '--------------------'.
               10  FILLER  PIC X(20) VALUE '----------  ---  ---'.
               10  FILLER  PIC X(20) VALUE '--         ----     '.
               10  FILLER  PIC X(20) VALUE '---     ----     ---'.
               10  FILLER  PIC X(20) VALUE '-     --------      '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(12) VALUE '            '.
      *---------------------------------------------------------------*
           05  TRAILER-1.
               10  FILLER  PIC X(20) VALUE '** TOTAL RECORDS PRO'.
               10  FILLER  PIC X(08) VALUE 'CESSED :'.
               10  TL1-RECORD-COUNT        PIC ZZ9.
               10  FILLER                  PIC X(101) VALUE SPACE.
      *---------------------------------------------------------------*
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
      *---------------------------------------------------------------*
       01  WS-NUMERIC-FIELDS.
           05  WS-CD-COST-NUM              PIC 9(03)V99.
           05  WS-TAX-NUM                  PIC 9(02)V99.
           05  WS-SHIPPING-COST-NUM        PIC 9(02)V99.
           05  WS-RECORD-COUNT             PIC 9(03) VALUE ZERO.
      *---------------------------------------------------------------*
       01  WS-SWITCHES-SUBSCRIPTS.
           05  END-OF-FILE-SW              PIC X VALUE 'N'.
               88  END-OF-FILE                   VALUE 'Y'.
      *---------------------------------------------------------------*
       01 PRINTER-CONTROL-FIELDS.
           05  LINE-SPACEING               PIC 9(02) VALUE 1.
           05  LINE-COUNT                  PIC 9(03) VALUE 999.
           05  LINES-ON-PAGE               PIC 9(02) VALUE 60.
           05  PAGE-COUNT                  PIC 9(02) VALUE 1.
           05  TOP-OF-PAGE                 PIC X(02) VALUE '1'.
           05  SINGLE-SPACE                PIC X(01) VALUE ' '.
           05  DOUBLE-SPACE                PIC X(01) VALUE '0'.
           05  TRIPLE-SPACE                PIC X(01) VALUE '-'.
           05  OVERPRINT                   PIC X(01) VALUE '+'.
      *---------------------------------------------------------------*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-PROCESS-FAVORITE-GROUPS.
      *---------------------------------------------------------------*
           PERFORM 1000-INITIALIZATION.
           PERFORM 8000-READ-FAV-GRP-FILE.
           PERFORM 2000-PROCESS-FAV-GRP-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-PRINT-TRAILER-LINES.
           PERFORM 4000-CLOSING.
           GOBACK.
      *---------------------------------------------------------------*
       1000-INITIALIZATION.
      *---------------------------------------------------------------*
           OPEN INPUT FAV-GRPS
                OUTPUT FAV-RPT.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR        TO HDR-YR.
           MOVE WS-CURRENT-MONTH       TO HDR-MO.
           MOVE WS-CURRENT-DAY         TO HDR-DAY.
      *---------------------------------------------------------------*
       2000-PROCESS-FAV-GRP-FILE.
      *---------------------------------------------------------------*
           MOVE ARTIST-NAME            TO WS-ARTIST-NAME.
           MOVE NUMBER-MUSICIANS       TO WS-NUMBER-MUSICIANS.
           MOVE MUSICAL-GENRE          TO WS-MUSICAL-GENRE.
           MOVE CD-COST                TO WS-CD-COST
                                          WS-CD-COST-NUM.
           MOVE SHIPPING-COST          TO WS-SHIPPING-COST
                                          WS-SHIPPING-COST-NUM.
           MOVE TAX                    TO WS-TAX
                                          WS-TAX-NUM.
           COMPUTE WS-TOTAL-COST = WS-CD-COST-NUM
                                 + WS-TAX-NUM
                                 + WS-SHIPPING-COST-NUM.
           MOVE BAND-IS-STILL-TOGETHER TO WS-BAND-STILL-TOGETHER .
           ADD 1                       TO WS-RECORD-COUNT.
           MOVE WS-PRINT-RECORD        TO NEXT-REPORT-LINE.
           PERFORM  9000-PRINT-REPORT-LINE.
           PERFORM  8000-READ-FAV-GRP-FILE.
      *---------------------------------------------------------------*
       3000-PRINT-TRAILER-LINES.
      *---------------------------------------------------------------*
           MOVE WS-RECORD-COUNT        TO TL1-RECORD-COUNT.
           MOVE TRAILER-1              TO NEXT-REPORT-LINE.
           MOVE 2                      TO LINE-SPACEING.
           PERFORM 9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
       4000-CLOSING.
      *---------------------------------------------------------------*
           CLOSE   FAV-GRPS
                   FAV-RPT.
      *---------------------------------------------------------------*
       8000-READ-FAV-GRP-FILE.
      *---------------------------------------------------------------*
           READ FAV-GRPS
               AT END MOVE 'Y' TO END-OF-FILE-SW.
      *---------------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
               PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *---------------------------------------------------------------*
           MOVE PAGE-COUNT           TO H1-PAGE-NUM.
           MOVE HEADING-LINE-1       TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                    TO LINE-SPACEING.
           MOVE HEADING-LINE-2       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                    TO LINE-SPACEING.
           MOVE HEADING-LINE-3       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD  1                    TO PAGE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
           MOVE 5                    TO LINE-COUNT.
      *---------------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                TO PRINT-LINE.
      *---------------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           MOVE SPACE                TO PRINT-LINE.
           ADD  1                    TO LINE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
      *--------------------------------------------------------------*
