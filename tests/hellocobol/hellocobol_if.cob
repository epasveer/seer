       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01_IF.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01  var-num1 PIC S9(9) VALUE 0.
           88  var-pass1 VALUES ARE 100 THRU 9999.
           01  var-num2 PIC S9(9) VALUE 1.
           88  var-pass2 VALUES ARE 100 THRU 9999.
           01  var-data PIC X(9) VALUE ' '.


       PROCEDURE DIVISION.

           DISPLAY 'ENTER number 1: '
           ACCEPT var-num1.

           DISPLAY 'ENTER number 2: '
           ACCEPT var-num2.

           DISPLAY 'ENTER some data: '
           ACCEPT var-data.       

           IF var-num1>var-num2 THEN
               DISPLAY 'Number1 is greater than Number2'
           ELSE
               IF var-num1 = var-num2 THEN
                   DISPLAY 'Number1 equals Number2'
               ELSE
                   DISPLAY 'Number1 is less than Number2'
               END-IF
           END-IF.


           IF var-num1 IS POSITIVE then
               DISPLAY 'Number1 is positive'
           END-IF.

           IF var-num1 IS NEGATIVE then
               DISPLAY 'Number1 is negative'
           END-IF.


             IF var-data IS NUMERIC THEN
      *    It's false because var-data is X(9).
               DISPLAY 'Numeric data'
           END-IF.

           IF var-data IS ALPHABETIC THEN
               DISPLAY 'Alphabetic data'
           END-IF.



           IF var-pass1 THEN
               DISPLAY 'Number1 is greater than 100'
           END-IF.

           IF NOT var-pass1 THEN
               DISPLAY 'Number1 is less than 100'
           END-IF.

           IF var-pass2 THEN
               DISPLAY 'Number2 is greater than 100'
           END-IF.

           IF NOT var-pass2 THEN
               DISPLAY 'Number2 is less than 100'
           END-IF.


           IF var-pass1 AND var-pass2 THEN
               DISPLAY 'Both of numbers are greater than 100'
           END-IF.

       STOP RUN.


       
