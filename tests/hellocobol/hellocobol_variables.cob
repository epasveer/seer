       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02_VARIABLES.
       
       DATA DIVISION.
           WORKING-STORAGE SECTION.
          
           01  struct-headers.
               02  filler          PIC x(2) VALUE "lp".
               02  filler          PIC x VALUE "|".
               02  filler          PIC x(10) VALUE "    number".
               02  filler          PIC x VALUE "|".
               02  filler          PIC x(10) VALUE "   decimal".
               02  filler          PIC x VALUE "|".
               02  filler          PIC x(10) VALUE "  currency".
              
              
           01  var-line                PIC x(80) VALUE ALL "-".
            
           01  struct-row.
               02  var-lp          PIC 9(2) VALUE 00.
               02  filler          PIC x VALUE "|".
               02  var-number      PIC z(10) VALUE 0.
               02  filler          PIC x VALUE "|".
               02  var-decimal     PIC +z(7).zz VALUE -317.21.
               02  filler          PIC x VALUE "|".
               02  var-currency    PIC $z(7).zz VALUE 317.21.
        
       PROCEDURE DIVISION.
           DISPLAY struct-headers.
           DISPLAY var-line.
            
           MOVE 01               TO var-lp.
           MOVE 3721             TO var-number.
            
           DISPLAY struct-row.
       STOP RUN.
            
