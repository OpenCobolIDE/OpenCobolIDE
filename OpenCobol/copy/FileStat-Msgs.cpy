      *> ***************************************************************
      *> ** Author: Gary L. Cutler                                    **
      *> **         CutlerGL@gmail.com                                **
      *> **                                                           **
      *> ** This copybook defines an EVALUATE statement capable of    **
      *> ** translating two-digit FILE-STATUS codes to a message.     **
      *> **                                                           **
      *> ** Use the REPLACING option to COPY to change the names of   **
      *> ** the MSG and STATUS identifiers to the names your program  **
      *> ** needs.                                                    **
      *> ***************************************************************
           EVALUATE STATUS
                WHEN 00 MOVE 'SUCCESS                  ' TO MSG   
                WHEN 02 MOVE 'SUCCESS DUPLICATE        ' TO MSG 
                WHEN 04 MOVE 'SUCCESS INCOMPLETE       ' TO MSG 
                WHEN 05 MOVE 'SUCCESS OPTIONAL         ' TO MSG 
                WHEN 07 MOVE 'SUCCESS NO UNIT          ' TO MSG 
                WHEN 10 MOVE 'END OF FILE              ' TO MSG 
                WHEN 14 MOVE 'OUT OF KEY RANGE         ' TO MSG 
                WHEN 21 MOVE 'KEY INVALID              ' TO MSG 
                WHEN 22 MOVE 'KEY EXISTS               ' TO MSG 
                WHEN 23 MOVE 'KEY NOT EXISTS           ' TO MSG 
                WHEN 30 MOVE 'PERMANENT ERROR          ' TO MSG 
                WHEN 31 MOVE 'INCONSISTENT FILENAME    ' TO MSG 
                WHEN 34 MOVE 'BOUNDARY VIOLATION       ' TO MSG 
                WHEN 35 MOVE 'FILE NOT FOUND           ' TO MSG 
                WHEN 37 MOVE 'PERMISSION DENIED        ' TO MSG 
                WHEN 38 MOVE 'CLOSED WITH LOCK         ' TO MSG 
                WHEN 39 MOVE 'CONFLICT ATTRIBUTE       ' TO MSG 
                WHEN 41 MOVE 'ALREADY OPEN             ' TO MSG 
                WHEN 42 MOVE 'NOT OPEN                 ' TO MSG 
                WHEN 43 MOVE 'READ NOT DONE            ' TO MSG 
                WHEN 44 MOVE 'RECORD OVERFLOW          ' TO MSG 
                WHEN 46 MOVE 'READ ERROR               ' TO MSG 
                WHEN 47 MOVE 'INPUT DENIED             ' TO MSG 
                WHEN 48 MOVE 'OUTPUT DENIED            ' TO MSG 
                WHEN 49 MOVE 'I/O DENIED               ' TO MSG 
                WHEN 51 MOVE 'RECORD LOCKED            ' TO MSG 
                WHEN 52 MOVE 'END-OF-PAGE              ' TO MSG 
                WHEN 57 MOVE 'I/O LINAGE               ' TO MSG 
                WHEN 61 MOVE 'FILE SHARING FAILURE     ' TO MSG 
                WHEN 91 MOVE 'FILE NOT AVAILABLE       ' TO MSG    
           END-EVALUATE.
