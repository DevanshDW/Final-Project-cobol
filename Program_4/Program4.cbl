       identification division.
       program-id. program4.
       author. Smit Patel. Devansh Patel.
       date-written. 10/04/2020.
      * Description : Produce a detailed report of R type records
      *
       environment division.
       input-output section.
       file-control.
           select data-file
               assign "../../../data/returns_records.dat"
               organization is line sequential.
      *
           select print-file
               assign "../../../data/returns_report.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd data-file
           data record is input-line
           record contains 36 characters.
      *
       01 input-line.
           05 input-rec-trans-code             pic x(1).
           05 input-rec-trans-amount           pic 9(5)v99.
           05 input-rec-pay-type               pic x(2).
               88 type-is-CA value "CA".
               88 type-is-CR value "CR".
               88 type-is-DB value "DB".
           05 input-rec-store-number           pic x(2).
           05 input-rec-invoice-number         pic x(9).
           05 input-rec-sku-code               pic x(15).
      *
       fd print-file
           record contains 73 characters
           data record is print-line.
      *
       01 print-line                           pic x(73).
      *
      * Working storage section
      *
       working-storage section.

       77 ws-eof-flag                          pic XX.
       77 ws-page-num                          pic 99 value 0.
       77 ws-line-count                        pic 99.
       77 ws-tax-indi                          pic 9(5)V99.
       77 ws-total-R                           pic 99 value 0.
       77 ws-total-cash                        pic 99 value 0.
       77 ws-total-credit                      pic 99 value 0.
       77 ws-total-debit                       pic 99 value 0.
       77 ws-total-cash-per                    pic 99V99 value 0.
       77 ws-total-credit-per                  pic 99V99 value 0.
       77 ws-total-debit-per                   pic 99V99.
       77 ws-total-tax                         pic 9(9)V99.
       77 ws-highest-trans-amount              pic 9(9)V99
           value zeroes.
       77 ws-lowest-trans-amount               pic 9(9)V99
           value zeroes.
       77 ws-highest-trans-store               pic 99 value 00.
       77 ws-lowest-trans-store                pic 99 value 00.
       77 ws-temp-total-trans                  pic 9(9)V99
           value zeroes.
       77 ws-total-trans-amount-calc           pic 9(6)V99.
       77 ws-tax-applicable                    pic 99 value 13.

       01 ws-total-tran-per-store occurs 6 times
           indexed by index-store.
           05 ws-total-tran-store              pic 9(9)V99
               value zeroes.

       01 ws-store-numbers.
           05 filler                           pic 99 value 01.
           05 filler                           pic 99 value 02.
           05 filler                           pic 99 value 03.
           05 filler                           pic 99 value 04.
           05 filler                           pic 99 value 05.
           05 filler                           pic 99 value 12.

       01 ws-store-num-const redefines ws-store-numbers
           occurs 6 times
           indexed by index-const              pic 99.

       01 ws-report-heading.
           05 filler                           pic x(25) value 
               "FINAL PROJECT - PROGRAM 4".
           05 filler                           pic x(28) value spaces.
           05 filler                           pic x(20) value 
               "Group 6 : Smit Patel".

       01 ws-report-heading-2.
           05 ws-rh-date                       pic 9(6) value zeroes.
           05 filler                           pic x(4) value spaces.
           05 ws-rh-time                       pic 9(7) value 
               zeroes.
           05 filler                           pic x(43) value spaces.
           05 filler                           pic x(13) value 
               "Devansh Patel".

       01 ws-page-heading.
           05 filler                           pic x(27) value spaces.
           05 filler                           pic x(14) value 
               "Returns Report".
           05 filler                           pic x(24) value spaces.
           05 filler                           pic x(6) value 
               "PAGE: ".
           05 ws-ch-page-no                    pic Z9 value "00".

       01 ws-report-header-1.
           05 filler                           pic X(5) value "Trans".
           05 filler                           pic X(3) value spaces.
           05 filler                           pic X(5) value "Trans".
           05 filler                           pic X(6) value spaces.
           05 filler                           pic X(7) value
               "Payment".
           05 filler                           pic X(2) value spaces.
           05 filler                           pic X(5) value "Store".
           05 filler                           pic X(3) value spaces.
           05 filler                           pic X(7) value
               "Invoice".
           05 filler                           pic X(5) value spaces.
           05 filler                           pic X(3) value "SKU".
           05 filler                           pic X(16) value spaces.
           05 filler                           pic X(3) value "Tax".
           05 filler                           pic X(4) value spaces.

       01 ws-report-header-2.
           05 filler                           pic X(4) value "Code".
           05 filler                           pic X(4) value spaces.
           05 filler                           pic X(6) value "Amount".
           05 filler                           pic X(5) value spaces.
           05 filler                           pic X(4) value "Type".
           05 filler                           pic X(5) value spaces.
           05 filler                           pic X(6) value "Number".
           05 filler                           pic X(2) value spaces.
           05 filler                           pic X(6) value "Number".
           05 filler                           pic X(6) value spaces.
           05 filler                           pic X(4) value "Code".
           05 filler                           pic X(15) value spaces.
           05 filler                           pic X(6) value "Amount".

       01 ws-report-header-3.
           05 filler                           pic X(4) value "----".
           05 filler                           pic X(4) value spaces.
           05 filler                           pic X(6) value "------".
           05 filler                           pic X(5) value spaces.
           05 filler                           pic X(4) value "----".
           05 filler                           pic X(5) value spaces.
           05 filler                           pic X(6) value "------".
           05 filler                           pic X(2) value spaces.
           05 filler                           pic X(6) value "------".
           05 filler                           pic X(6) value spaces.
           05 filler                           pic X(4) value "----".
           05 filler                           pic X(15) value spaces.
           05 filler                           pic X(6) value "------".

       01 ws-report-details.
           05 filler                           pic X(2) value spaces.
           05 ws-transaction-code              pic X.
           05 filler                           pic X(2) value spaces.
           05 ws-transaction-amount            pic Z(4)9.99.
           05 filler                           pic X(1) value '$'.
           05 filler                           pic X(5) value spaces.
           05 ws-payment-type                  pic XX.
           05 filler                           pic X(7) value spaces.
           05 ws-store-number                  pic 99.
           05 filler                           pic X(6) value spaces.
           05 ws-invoice-number                pic X(9).
           05 filler                           pic X(3) value spaces.
           05 ws-sku-code                      pic X(15).
           05 filler                           pic X(1) value spaces.
           05 ws-taxes                         pic Z(4)9.99.
           05 filler                           pic X(1) value '$'.

       01 ws-empty-line.
           05 filler                           pic x(73) value spaces.

       01 ws-tno-R.
           05 filler                           pic x(35) value 
               "Total number of R records    : ".
           05 ws-tno-R-val                     pic z9.
           05 filler                           pic x(36) value spaces.

       01 ws-total-trans-amount.
           05 filler                           pic x(34) value 
               "Total transaction Amount     :    ".
           05 ws-tt-amount-val                 pic $$,$$9.99.

       01 ws-payment-t-per.
           05 filler                           pic x(34) value 
               "Payment Types Percentage: CASH- ".
           05 ws-tper-cash-val                 pic z9.99.
           05 filler                           pic x(12) value 
               "%   CREDIT- ".
           05 ws-tper-credit-val               pic z9.99.
           05 filler                           pic x(11) value 
               "%   DEBIT- ".
           05 ws-tper-debit-val                pic z9.99.
           05 filler                           pic x(4) value '%'.

       01 ws-total-tax-owing.
           05 filler                           pic x(33) value
               "Total tax owing              :".
           05 ws-total-tax-value               pic $$,$$9.99.

       01 ws-store-num-with-highest-R.
           05 filler                           pic x(47) value 
               "Store number with highest return transaction - ".
           05 ws-snum-w-h-R-val                pic Z9.

       01 ws-store-num-with-lowest-R.
           05 filler                           pic x(47) value 
               "Store number with lowest  return transaction - ".
           05 ws-snum-w-l-R-val                pic Z9.

       01 ws-store-totals-heading.
           05 filler                           pic x(2) value spaces.
           05 filler                           pic x(12) value 
               "Store #1".
           05 filler                           pic x(12) value 
               "Store #2".
           05 filler                           pic x(12) value 
               "Store #3".
           05 filler                           pic x(12) value 
               "Store #4".
           05 filler                           pic x(12) value 
               "Store #5".
           05 filler                           pic x(12) value 
               "Store #12".

       01 ws-store-totals.
           05 ws-total-line-r-store-num
               occurs 6 times indexed by ws-tl-r-index.
               10 ws-r-trans-total             pic z,zz9.99.
               10 filler                       pic x(1) value '$'.
               10 filler                       pic x(3) value spaces.

       procedure division.
       0100-read-EMPLOYEES.
      *
      * Open data and outout files
      *
           open input data-file.
           open output print-file.

           accept ws-rh-date from date.
           accept ws-rh-time from time.
      *
      * Write report main heading
      *
           write print-line            from ws-report-heading.
           write print-line            from ws-report-heading-2.

           read data-file
               at end
                   move 't'            to ws-eof-flag
           end-read.

           perform 110-process-lines   until ws-eof-flag = 't'.

           perform 120-print-footer.

      * Close files
           close data-file print-file.
           goback.

       110-process-lines.

           perform 130-print-heading.

           perform 140-line-on-a-page
               varying ws-line-count from 1 by 1
               until ws-line-count > 20
               or ws-eof-flag = "t".
      *
      * Print Heading
      *
       130-print-heading.

           add 1 to ws-page-num.
           move ws-page-num to ws-ch-page-no.
      * Write report header
           write print-line from ws-page-heading
             after advancing 1 line.
           write print-line from ws-report-header-1
             after advancing 2 line.
           write print-line from ws-report-header-2
             after advancing 1 line.
           write print-line from ws-report-header-3
             after advancing 1 line.
           write print-line from ws-empty-line
             after advancing 1 line.
      *
      * Print Footer
      *
       120-print-footer.

           perform 150-processing-variables.
      * Copy transaction amounts to output variables
           perform 125-moving-totals varying ws-tl-r-index
               from 1 by 1
               until ws-tl-r-index = 7.

           move ws-total-trans-amount-calc
               to ws-tt-amount-val.
           move ws-total-R to ws-tno-R-val
           write print-line from ws-tno-R
               after advancing 2 line.
           write print-line from ws-total-trans-amount
             after advancing 2 line.

           write print-line from ws-store-totals-heading
               after advancing 2 line.
           write print-line from ws-store-totals
             after advancing 1 line.

           move ws-total-cash-per to ws-tper-cash-val.
           move ws-total-credit-per to ws-tper-credit-val.
           move ws-total-debit-per to ws-tper-debit-val.
           write print-line from ws-payment-t-per
             after advancing 2 line.

           move ws-total-tax to ws-total-tax-value.
           write print-line from ws-total-tax-owing
             after advancing 2 line.

           move ws-highest-trans-store to ws-snum-w-h-R-val.
           write print-line from ws-store-num-with-highest-R
             after advancing 2 line.

           move ws-lowest-trans-store to ws-snum-w-l-R-val.
           write print-line from ws-store-num-with-lowest-R
             after advancing 1 line.
      *
      * Moving totals to output variables
      *
       125-moving-totals.
           set index-store to ws-tl-r-index.
           move ws-total-tran-store(index-store)
               to ws-r-trans-total(ws-tl-r-index).
           add ws-total-tran-store(index-store)
               to ws-total-trans-amount-calc.

       140-line-on-a-page.
           add 1 to ws-total-R.
           perform 160-print-lines.

           read data-file
               at end
                   move "t" to ws-eof-flag
           end-read.
      *
      * Calculating Total for Cash, Credit and Debit
      *
       150-processing-variables.

           compute ws-total-cash-per rounded =
               (ws-total-cash * 100) / ws-total-R.

           compute ws-total-credit-per rounded =
               (ws-total-credit * 100) / ws-total-R.

           compute ws-total-debit-per rounded =
               (ws-total-debit * 100) / ws-total-R.
      *
      * Calculating tax
      *
       160-print-lines.

           if (type-is-CA) then
               add 1                       to ws-total-cash
           else if (type-is-CR) then
               add 1                       to ws-total-credit
           else if (type-is-DB) then
               add 1                       to ws-total-debit
           end-if.

           set index-store                 to 1

           perform 170-process-stores varying index-store from 1 BY 1
               until index-store = 7.

           compute ws-tax-indi rounded =
               (input-rec-trans-amount * ws-tax-applicable) / 100.

           add ws-tax-indi                 to ws-total-tax.

           move input-rec-trans-code       to ws-transaction-code.
           move input-rec-trans-amount     to ws-transaction-amount.
           move input-rec-pay-type         to ws-payment-type.
           move input-rec-store-number     to ws-store-number.
           move input-rec-invoice-number   to ws-invoice-number.
           move input-rec-sku-code         to ws-sku-code.
           move ws-tax-indi                to ws-taxes.

           write print-line from ws-report-details
               after advancing 1 line.
      *
      * Processing each store
      *
       170-process-stores.
           set index-const to index-store.
           if (input-rec-store-number =
               ws-store-num-const(index-const) ) then

               add input-rec-trans-amount
                   to ws-total-tran-store(index-store)

               move ws-total-tran-store(index-store)
                   to ws-temp-total-trans

               if (ws-temp-total-trans > ws-highest-trans-amount) then
                   move ws-store-num-const(index-const)
                       to ws-highest-trans-store
                   move ws-temp-total-trans
                       to ws-highest-trans-amount
               end-if

               move ws-highest-trans-amount to ws-lowest-trans-amount

               if (ws-temp-total-trans < ws-lowest-trans-amount) then
                   move ws-store-num-const(index-const)
                       to ws-lowest-trans-store
                   move ws-temp-total-trans
                       to ws-lowest-trans-amount
               end-if
           end-if.

       end program program4.