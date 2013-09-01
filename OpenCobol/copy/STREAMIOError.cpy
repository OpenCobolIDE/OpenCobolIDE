      *> ***************************************************************
      *> ** Author: Gary L. Cutler                                    **
      *> **         CutlerGL@gmail.com                                **
      *> **                                                           **
      *> ** This copybook defines an error-handling routine for use   **
      *> ** with the STREAMIO subroutine (STREAMIO.cbl).  See the     **
      *> ** comments in that program for a description of the         **
      *> ** functionality provided by the STREAMIO package.           **
      *> ***************************************************************
       ENTRY "STREAMIOError"
       >>SOURCE FREE
       DISPLAY " "                                                  UPON SYSERR END-DISPLAY
       DISPLAY "*** STREAMIO ERROR ***"                             UPON SYSERR END-DISPLAY
       DISPLAY " "                                                  UPON SYSERR END-DISPLAY
       DISPLAY "File:        " FUNCTION TRIM(SCB-Filename,TRAILING) UPON SYSERR END-DISPLAY
       DISPLAY "Function:    " SCB-Function                         UPON SYSERR END-DISPLAY
       DISPLAY "Mode:        " SCB-Mode                             UPON SYSERR END-DISPLAY
       DISPLAY "Delimiter:   " SCB-Delimiter-Mode                   UPON SYSERR END-DISPLAY
       DISPLAY "Offset:      " SCB-Offset                           UPON SYSERR END-DISPLAY
       DISPLAY "Return Code: " SCB-Return-Code                      UPON SYSERR END-DISPLAY
       .
       >>SOURCE FIXED
