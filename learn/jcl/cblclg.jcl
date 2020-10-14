//CBLCLG JOB 1,NOTIFY=&SYSUID
//***************************************************/
// SET COBPGM='FAVRPT'
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..LEARN.CBL(&COBPGM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LEARN.LOAD(&COBPGM),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=&COBPGM
//STEPLIB   DD DSN=&SYSUID..LEARN.LOAD,DISP=SHR
//FAVIN     DD DSN=&SYSUID..LEARN.DATA(FAVIN),DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
