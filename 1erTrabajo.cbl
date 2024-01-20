      ******************************************************************
      * Author:Pedro Burgos
      * Date: 4/1/24
      * Purpose:Reconocimiento
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      /SECCION DE IDENTIFICATION

       PROGRAM-ID. MI-APP.
       AUTHOR PEDRO BURGOS.

       INSTALLATION LINKEDIN PEDRO BURGOS.
      /INSTALACION SERIA LA EMPRESA DONDE SE TRABAJA.

       ENVIRONMENT DIVISION.
      /CONFIGURA EL ENTORNO
      /ARCHIVOS , DISPOSITIVOS IN/OUT + CARACTERISTICAS

       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
      /DONDE DECLARO ARCHIVOS Y VARIABLES

       FILE SECTION.
      /ARCHIVOS

       WORKING-STORAGE SECTION.
      /VARIABLES


       01 NU PIC X(10).
      / Definicion de una variable llamada NU(Nombre-Usuario)
      / Del tipo alfanumerico(X) con una longifud maxima de 10 caracteres
      / 01:Indica que estamos declarando una nueva variable
      / 01:sugiere que estamos definiendo la primera vairable o un grupo de estas
      /  PICTURE define el formato/estructura de la VARIABLE
      /PIC X significa la varaible es alfanumerica

       PROCEDURE DIVISION.
      /INCIO DE LA SECCION DE PROCEDIMIENTOS

       MAIN-PROCEDURE.
      /INICIO DEL PROCEDIMIENTO PRINCIPAL
            DISPLAY "Ingrese su NU: ".
            ACCEPT NU.
      /ACEPTO LA ENTRADA DE USUARIO Y LA ALMACENA EN LA VARIABLE NU

            DISPLAY SPACE.
            DISPLAY "HOLA "NU".BIENVENIDX A COBOL".
            STOP RUN.
       END PROGRAM MI-APP.
