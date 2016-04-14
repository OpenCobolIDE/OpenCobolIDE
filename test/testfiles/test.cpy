       78  txt-lower          value 'abcdefghijklmnopqrstuvwxyzäöü'.
       78  txt-upper          value 'ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ'.
      *
       01  var-a.
           05  filler     pic x(10).
               88  var-a-1-a     value '1.Here'.
               88  var-a-1-b     value '1.There'.
           05  filler     pic x(09).
           05  filler     pic x(09).
           05  filler     pic x(09).
           05  filler     pic x(06).
           05  filler     pic x(10).
           05  filler     pic x(06).
           05  filler     pic x(06).
           05  filler     pic x(05).
           05  filler     pic x(08).
               88  var-a-10-a    value '10.fish'.
               88  var-a-10-b    value '10.sheep'.
               88  var-a-10-no   value spaces.
      *
       78  myexp-1                     value 'save record and go back to
      -                                      ' the main menu'.
       78  myconst-1                   value 'Main'.
       78  myconst-2                   value 'Cancel'.
