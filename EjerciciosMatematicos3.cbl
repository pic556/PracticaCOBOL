      ******************************************************************
      * Author:Pedro Burgos
      * Date:11/1/24
      * Purpose:Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA3.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 Num1 PIC 9(3).
       01 Fac PIC 9(6) COMP-5 VALUE 1.

       01 Num2 PIC 9(3).
       01 SumI PIC 9(3) VALUE 0.
       01 Ind  PIC 9(3) VALUE 1.

       01 Res_Resto PIC 9(3).
       01 BOOLEAN-VARIABLE   PIC 9 VALUE 0.




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Ingrese su NU1: ".
           ACCEPT Num1.
           DISPLAY "Ingrese su NU2: ".
           ACCEPT Num2.




           PERFORM Fact.
           DISPLAY SPACE.
           DISPLAY "EL FACTORIAL DE NU1 ES: " Fac.

      /FACT de factorial



           PERFORM SICSM.
      /SICSM. es sumaImparesCuyoCuadSeaMenorque
      /Escribir una funcion que sume sus impares ,que sean menores
      /al cuadrado de esta , ejemplo de 30 solo tengo
      / 1 + 3 + 5 = 9
           DISPLAY "LA sumaImparesCuyoCuadSeaMenorqueNu1 es  : " SumI.


           STOP RUN.

       Fact.
              IF Num1 NOT = 1 THEN
                     COMPUTE Fac = Fac * Num1
                     COMPUTE Num1 = Num1 - 1
                     PERFORM Fact
              END-IF.




       SICSM.
              IF Ind < Num2 THEN
                            IF Ind**2 < Num2 THEN
                                  COMPUTE SumI = SumI + Ind
                                  COMPUTE ind = Ind + 2
                                  PERFORM SICSM
                            ELSE
                                   COMPUTE Ind = ind + 4
                                   PERFORM SICSM

                            END-IF
              END-IF.


       END PROGRAM MATEMATICA3.
