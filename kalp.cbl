       IDENTIFICATION DIVISION.                                           
       PROGRAM-ID. MAXLOAN-CALC.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           *> Variabler som är bestämda innan programmet startar.
           78  CUR VALUE "SEK".

           01  FIRST-TIME-FLAG PIC 9 VALUE 1.
           
           01  USER-INPUT-X PIC X(20).
                   88  USER-WANTS-TO-STOP VALUE "NEJ".
           01  USER-INPUT-9 PIC 9(10).
           
           *> Tar hänsyn till konsumentverkets skäliga levnadskostnader. Kostnader per månad i SEK.
           *> https://www.konsumentverket.se/ekonomi/vilka-kostnader-har-ett-hushall/
           78  EXPENCE-FOR-ONE-GROWN VALUE 11000.
           78  EXPENCE-PER-GROWN VALUE 9700.
           78  EXPENCE-PER-CHILD VALUE 4950.
           78  EXPENCE-PER-OWNED-CAR VALUE 1700.

           *> Vi räknar vår kalkyl med en låneperiod på 480 månader (40 år).
           78 LOAN-PERIOD-IN-MONTHS VALUE 480.

           *> 30% Skatt på lön
           78 TAX-RATE VALUE 0.30.

           *> Vi räknar med 6.5% ränta som worst-case scenario. Det är vad vi kan uppskatta räntan att stiga till som HÖGST.
           01  INTEREST-RATE PIC 9V999 VALUE 0.065.
           
           01  MONTHLY-RATE PIC 9(5)V9999.

           01  REMAINDER-AFTER-EXPENSES PIC 9(6).

           01  POW-VALUE PIC 9(10)V9999.

           01  DENOMINATOR PIC 9(10)V9999.

           01  MAX-LOAN-AMOUNT PIC 9(10).
           01  DISPLAY-MAX-LOAN-AMOUNT PIC ZZZ,ZZZ,ZZ9.99.

           01  MAX-LOAN-BASED-ON-DEPOSIT PIC 9(10).

           01  MAX-PROPERTY-PRICE PIC 9(10).
           01  DISPLAY-MAX-PROPERTY-PRICE PIC ZZZ,ZZZ,ZZ9.99.

           01  OTHER-LOAN-EXPENSES PIC 9(6).
           01  CAR-EXPENSES PIC 9(5).
           01  FAMILY-MEMBERS-EXPENSES PIC 9(5).
           01  PROPERTY-EXPENSES PIC 9(5).
           01  TOTAL-EXPENSES PIC 9(7).

           01  DISPLAY-CASH-DEPOSIT.
               02 DEPOSIT-PERCENT PIC 99V999.
               02 LOAN-PERCENT PIC 99V999.
           
           *> Variabler som användare skriver in i programmet
           01 GROSS-SALARY PIC 9(6).
           01 NET-SALARY PIC 9(6).
           01 CASH-DEPOSIT PIC 9(7).

           01 MONTHLY-FEE PIC 9(5).
               88 VILLA VALUE 4500.
               88 COOPERATIVE-HOUSING VALUE 500.
       
           01  CARS.
               02  CARS-OWNED PIC 9(2).
               02  CARS-LEASED-TOTAL-PRICE PIC 9(5).
               
           01 FAMILY-MEMBERS.
               02 TOTAL-GROWN PIC 9(2).
               02 TOTAL-CHILDREN PIC 9(2).

           
       PROCEDURE DIVISION.
           PERFORM CALCULATE-DEPOSIT-AND-SALARY.
           PERFORM CALCULATE-PROPERTY-EXPENSES.
           PERFORM CALCULATE-FAMILY-MEMBERS-EXPENSES.
           PERFORM CALCULATE-PERSONAL-EXPENSES.
           PERFORM CALCULATE-MAXIMUM-LOAN.
       
       CALCULATE-DEPOSIT-AND-SALARY SECTION.
           DISPLAY "Total inkomst före skatt?: " WITH NO ADVANCING
           ACCEPT USER-INPUT-9
           MOVE USER-INPUT-9 TO GROSS-SALARY
           COMPUTE NET-SALARY = GROSS-SALARY - (GROSS-SALARY * TAX-RATE)

           DISPLAY "Hur stor kontantinsats vill du betala: " WITH NO ADVANCING
           ACCEPT USER-INPUT-9
           MOVE USER-INPUT-9 TO CASH-DEPOSIT
           EXIT.
       
       CALCULATE-PROPERTY-EXPENSES SECTION.
           DISPLAY "Villa eller Bostadsrätt? (Bostadsrätt/Villa):" WITH NO ADVANCING
           ACCEPT USER-INPUT-X

           INSPECT USER-INPUT-X CONVERTING 'abcdefghijklmnopqrstuvwxyz' TO'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           IF USER-INPUT-X = "VILLA" THEN
               SET VILLA TO TRUE
           ELSE
               SET COOPERATIVE-HOUSING TO TRUE
           END-IF
           MOVE MONTHLY-FEE TO PROPERTY-EXPENSES
           EXIT.

       CALCULATE-FAMILY-MEMBERS-EXPENSES SECTION.
           DISPLAY "Hur många vuxna i hushållet?: " WITH NO ADVANCING
           ACCEPT USER-INPUT-9
           ADD USER-INPUT-9 TO TOTAL-GROWN
       
           DISPLAY "Hur många barn under 20 år i hushållet?: " WITH NO ADVANCING
           ACCEPT USER-INPUT-9
           ADD USER-INPUT-9 TO TOTAL-CHILDREN

           IF TOTAL-GROWN = 1 AND TOTAL-CHILDREN = 0 THEN
               COMPUTE FAMILY-MEMBERS-EXPENSES = EXPENCE-FOR-ONE-GROWN
           ELSE           
               COMPUTE FAMILY-MEMBERS-EXPENSES = (TOTAL-GROWN * EXPENCE-PER-GROWN) + (TOTAL-CHILDREN * EXPENCE-PER-CHILD)
           END-IF.

           DISPLAY FAMILY-MEMBERS-EXPENSES.
           EXIT.
       
           
       CALCULATE-PERSONAL-EXPENSES SECTION.
           PERFORM UNTIL USER-WANTS-TO-STOP
               IF FIRST-TIME-FLAG = 1 THEN
                   MOVE 0 TO FIRST-TIME-FLAG
                   DISPLAY "Äger du en bil? (Ja/Nej): " WITH NO ADVANCING
               ELSE
                   DISPLAY "Äger du en annan bil? (Ja/Nej): " WITH NO ADVANCING
               END-IF
               
               ACCEPT USER-INPUT-X
               *> Convert input to uppercase and trim spaces
               INSPECT USER-INPUT-X CONVERTING 'abcdefghijklmnopqrstuvwxyz' TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               MOVE FUNCTION TRIM(USER-INPUT-X) TO USER-INPUT-X
               
               *> Check if user wants to stop
               IF USER-WANTS-TO-STOP THEN
                   EXIT PERFORM
               END-IF
               
               DISPLAY "Äger du eller leasar du bilen? (Lease/Köpt): " WITH NO ADVANCING
               ACCEPT USER-INPUT-X
               
               *> Convert input to uppercase and trim spaces
               INSPECT USER-INPUT-X CONVERTING 'abcdefghijklmnopqrstuvwxyz' TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               MOVE FUNCTION TRIM(USER-INPUT-X) TO USER-INPUT-X
               
               IF USER-INPUT-X = "LEASE" THEN
                   DISPLAY "Hur mycket kostar den i månaden?: " WITH NO ADVANCING
                   ACCEPT USER-INPUT-9
                   ADD USER-INPUT-9 TO CARS-LEASED-TOTAL-PRICE
               ELSE
                   ADD 1 TO CARS-OWNED
               END-IF
           
           END-PERFORM.

           DISPLAY "Har mycket betalar du för andra lån eller krediter som t.ex. studielån, billån eller kortkrediter?: " WITH NO ADVANCING
           ACCEPT USER-INPUT-9
           MOVE USER-INPUT-9 TO OTHER-LOAN-EXPENSES
       
           *> Här kalkylerar vi den totala bil kostnaden
           COMPUTE CAR-EXPENSES = (CARS-OWNED * EXPENCE-PER-OWNED-CAR) + CARS-LEASED-TOTAL-PRICE.
           EXIT.

       CALCULATE-MAXIMUM-LOAN SECTION.
           COMPUTE TOTAL-EXPENSES = CAR-EXPENSES + FAMILY-MEMBERS-EXPENSES + OTHER-LOAN-EXPENSES + PROPERTY-EXPENSES
           COMPUTE REMAINDER-AFTER-EXPENSES = NET-SALARY - TOTAL-EXPENSES

           COMPUTE MONTHLY-RATE = INTEREST-RATE / 12
           COMPUTE POW-VALUE = FUNCTION EXP( LOAN-PERIOD-IN-MONTHS * FUNCTION LOG( 1 + MONTHLY-RATE ) ).
           COMPUTE DENOMINATOR = MONTHLY-RATE * ( POW-VALUE / ( POW-VALUE - 1 ) ).
           
           IF DENOMINATOR = 0 THEN
               MOVE 0 TO MAX-LOAN-AMOUNT
           ELSE
               COMPUTE MAX-LOAN-BASED-ON-DEPOSIT = (CASH-DEPOSIT  * 100) / 15
               COMPUTE MAX-LOAN-AMOUNT = REMAINDER-AFTER-EXPENSES / DENOMINATOR
               
               IF MAX-LOAN-AMOUNT > MAX-LOAN-BASED-ON-DEPOSIT THEN
                   MOVE MAX-LOAN-BASED-ON-DEPOSIT TO MAX-LOAN-AMOUNT
               END-IF
           END-IF


           COMPUTE MAX-PROPERTY-PRICE = MAX-LOAN-AMOUNT + CASH-DEPOSIT
           MOVE MAX-LOAN-AMOUNT TO DISPLAY-MAX-LOAN-AMOUNT
           MOVE MAX-PROPERTY-PRICE TO DISPLAY-MAX-PROPERTY-PRICE

           COMPUTE DEPOSIT-PERCENT = (CASH-DEPOSIT / MAX-PROPERTY-PRICE) * 100
           COMPUTE LOAN-PERCENT = 100 - DEPOSIT-PERCENT

           DISPLAY "Du kan låna MAX " FUNCTION TRIM(DISPLAY-MAX-LOAN-AMOUNT) " " CUR " och du kan köpa för MAX " FUNCTION TRIM(DISPLAY-MAX-PROPERTY-PRICE)
           DISPLAY "Du betalar " DEPOSIT-PERCENT "% i kontantinsats, och lånar " LOAN-PERCENT "%"
       EXIT.
               
       STOP RUN.
       