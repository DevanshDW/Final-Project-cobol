       identification division.
       program-id. program3.
       author.Smit Patel. Devansh Patel.
       date-written. 10/04/2020.
      * Description : Produce a detailed report of sales
      *
       environment division.
       input-output section.
       file-control.

           select input-file
               assign "../../../data/s_l_records.dat"
               organization is line sequential.
           select output-file
               assign "../../../data/s_l_report.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd input-file
           data record is input-line
           record contains 36 characters.
      *
       01 input-line.
           05 input-rec-trans-code             pic x(1).
               88 transac-is-s value "S".
               88 transac-is-r value "R".
               88 transac-is-l value "L".
           05 input-rec-trans-amount           pic 9(5)v99.
           05 input-rec-pay-type               pic x(2).
               88 type-is-CA value "CA".
               88 type-is-CR value "CR".
               88 type-is-DB value "DB".
           05 input-rec-store-number           pic x(2).
           05 input-rec-invoice-number         pic x(9).
           05 input-rec-sku-code               pic x(15).
      *
       fd output-file
           record contains 73 characters
           data record is print-line.
      *
       01 print-line                           pic x(73).
      *
       working-storage section.
      *
       77 WS-EOF-FLAGS                         pic XX.
      *
       77 ws-pg-numbers                        pic 99 value 0.
       77 ws-c-for-lines                       pic 99 value zeroes.
       77 ws-tax-for-each-person               pic 9(5)V99.
       77 ws-total-for-s                       pic 99 value zeroes.
       77 ws-total-l                           pic 99 value zeroes.
       77 ws-total-for-sl                      pic 99 value zeroes.
       77 ws-total-for-cash                    pic 99 value zeroes.
       77 ws-total-for-credit                  pic 99 value zeroes.
       77 ws-total-for-debit                   pic 99 value zeroes.
       77 ws-total-percentage-of-cash          pic 99V99 value zeroes.
       77 ws-total-percentage-of-credit        pic 99V99 value zeroes.
       77 ws-total-percentage-of-debit         pic 99V99 value zeroes.
       77 ws-tax-in-total                      pic 9(9)V99 value zero.
       77 ws-total-number-of-stores            pic 99 value 06.
       77 ws-total-transaction-initially       pic 9(9)V99 value 0.
       77 ws-maximum-amount-of-trans           pic 9(9)V99 value 0.
       77 ws-minimum-amount-of-trans           pic 9(9)V99 value 0.
       77 ws-max-no-of-transaction-store       pic 99 value 00.
       77 ws-min-no-of-transaction-store       pic 99 value 00.
      *
       01 ws-total-trans-store-indi        occurs 6 times 
           indexed by indx-for-store.
           05 ws-final-transaction-store       pic 9(9)V99 value 0.
      *
       01 ws-num-for-stores.
           05 filler                           pic 99 value 01.
           05 filler                           pic 99 value 02.
           05 filler                           pic 99 value 03.
           05 filler                           pic 99 value 04.
           05 filler                           pic 99 value 05.
           05 filler                           pic 99 value 12.
      *
       01 ws-store-num-const redefines ws-num-for-stores
           occurs 6 times
           indexed by index-const              pic 99.
      *
       01 ws-constants.
           05 ws-tax-applicable                pic 99 value 13.
      *
       01 ws-r-heading.
           05 filler                           pic x(25) value 
               "FINAL PROJECT - PROGRAM 3".
           05 filler                           pic x(28) value spaces.
           05 filler                           pic x(20) value 
               "Group 6 : Smit Patel".
      *
       01 ws-r-heading-2.
           05 ws-rh-date                       pic 9(6) value zeroes.
           05 filler                           pic x(4) value spaces.
           05 ws-rh-time                       pic 9(7) value zeroes.
           05 filler                           pic x(43) value spaces.
           05 filler                           pic x(13) value 
               "Devansh Patel".
      *
       01 ws-heding-for-page.
           05 filler                           pic x(31) value spaces.
           05 ws-heading-pg                    pic x(12) value 
               "S&L Result".
           05 filler                           pic x(22) value spaces.
           05 filler                           pic x(6)  value
               "PAGE :".
           05 ws-c-pg-number                   pic Z9    value "00".
      *
       01 ws-r-heading-for-line-1.
           05 filler                           pic x(4)  value "Trns".
           05 filler                           pic x(5)  value spaces.
           05 filler                           pic x(4)  value "Trns".
           05 filler                           pic x(6)  value spaces.
           05 filler                           pic x(7)  value 
               "Payment".
           05 filler                           pic x(2)  value spaces.
           05 filler                           pic x(5)  value "Store".
           05 filler                           pic x(3)  value spaces.
           05 filler                           pic x(7)  value 
               "Invoice".
           05 filler                           pic x(5)  value spaces.
           05 filler                           pic x(3)  value "SKU".
           05 filler                           pic x(16) value spaces.
           05 filler                           pic x(3)  value "Tax".
      *
       01 ws-r-heading-for-line-2.
           05 filler                           pic x(4)  value "Code".
           05 filler                           pic x(5)  value spaces.
           05 filler                           pic x(6)  value
               "Amount".
           05 filler                           pic x(4)  value spaces.
           05 filler                           pic x(4)  value "Type".
           05 filler                           pic x(5)  value spaces.
           05 filler                           pic x(6)  value
               "Number".
           05 filler                           pic x(2)  value spaces.
           05 filler                           pic x(6)  value
               "Number".
           05 filler                           pic x(6)  value spaces.
           05 filler                           pic x(4)  value "Code".
           05 filler                           pic x(15) value spaces.
           05 filler                           pic x(6)  value
               "Amount".
      *
       01 ws-r-heading-for-line-dash.
           05 filler                           pic x(4)  value "----".
           05 filler                           pic x(5)  value spaces.
           05 filler                           pic x(6)  value "------".
           05 filler                           pic x(4)  value spaces.
           05 filler                           pic x(7)  value 
               "-------".
           05 filler                           pic x(2)  value spaces.
           05 filler                           pic x(6)  value "------".
           05 filler                           pic x(2)  value spaces.
           05 filler                           pic x(6)  value "------".
           05 filler                           pic x(6)  value spaces.
           05 filler                           pic x(4)  value "----".
           05 filler                           pic x(15) value spaces.
           05 filler                           pic x(6)  value "------".
      *
       01 ws-repo-info.
           05 filler                           pic x(1)  value spaces.
           05 ws-trans-code                    pic xx.
           05 filler                           pic x(4)  value spaces.
           05 ws-trans-amnt                    pic $(4)9.99.
           05 filler                           pic x(4)  value spaces.
           05 ws-typ-of-paymnt                 pic XX.
           05 filler                           pic x(7)  value spaces.
           05 ws-num-of-str                    pic 99.
           05 filler                           pic x(6)  value spaces.
           05 ws-invc-num                      pic x(9).
           05 filler                           pic x(3)  value spaces.
           05 ws-sku-c                         pic x(15).
           05 filler                           pic x(2)  value spaces.
           05 ws-taxe                          pic $(4)9.99.
           05 filler                           pic x(1)  value spaces.
      *
       01 ws-blank-lines.
           05 filler                           pic x(73) value spaces.
      *
       01 ws-tno-s-and-l.
           05 filler                           pic x(35)
               value "Total number of S&L records    : ".
           05 ws-tno-s-and-l-val               pic z9.
           05 filler                           pic x(36) value spaces.
      *
       01 ws-tno-s.
           05 filler                           pic x(35)
               value "Total number of S   records    : ".
           05 ws-tno-s-val                     pic z9.
           05 filler                           pic x(36) value spaces.
      *
       01 ws-tno-l.
           05 filler                           pic x(35)
               value "Total number of L   records    : ".
           05 ws-tno-l-val                     pic x9.
           05 filler                           pic x(36) value spaces.
      *
       01 ws-payment-t-per.
           05 filler                           pic x(33) value
               "Payment Types Percentage:  CASH: ".
           05 ws-tper-cash-val                 pic z9.99.
           05 filler                           pic x(13) value
               "%    CREDIT: ".
           05 ws-tper-credit-val               pic z9.99.
           05 filler                           pic x(11) value
               "%    DEBIT: ".
           05 ws-tper-debit-val                pic z9.99.
           05 filler                           pic x(1) value '%'.
      *
       01 ws-tax-in-total-owing.
           05 filler                           pic x(32) value
               "Total tax owing                :".
           05 filler                           pic x(3) value spaces.
           05 ws-tax-in-total-value            pic $$,$$9.99.
      *
       01 ws-store-num-with-highest-sl.
           05 filler                           pic x(49) value 
               "Store num with highest S&L transaction Amount -  ".
           05 ws-snum-w-h-sl-val               pic Z9.
      *
       01 ws-store-num-with-lowest-sl.
           05 filler                           pic x(49) value 
               "Store num with lowest S&L  transaction Amount -  ".
           05 ws-snum-w-l-sl-val               pic Z9.
      *
      * Procedure Division
      *
       procedure division.
       000-main.
      * Open files
           open input  input-file.
           open output output-file.
      * Move date and time
           accept ws-rh-date from date.
           accept ws-rh-time from time.
      * Write report heading
           write print-line from ws-r-heading
               after advancing 0 line.
           write print-line from ws-r-heading-2
               after advancing 1 line.
      * Start reading input file
           read input-file
               AT END move 't'             to WS-EOF-FLAGS
           end-read.
      * Process records
           perform 100-process-line until WS-EOF-FLAGS = 't'.

           perform 150-print-footer.

      * Close files and go back
           close input-file output-file.
           goback.
      *
      * Process Each Line
      *
       100-process-line.

           perform 110-print-heading.

           perform 120-line-on-a-page
               varying ws-c-for-lines from 1 by 1
               until   ws-c-for-lines > 20
               or      WS-EOF-FLAGS = "t".
      *
      * Print Heading
      *
       110-print-heading.
           add 1                           to ws-pg-numbers.
           move ws-pg-numbers              to ws-c-pg-number.
      * Write headers
           write print-line    from ws-heding-for-page
               after advancing 2 line.
           write print-line    from ws-r-heading-for-line-1
               after advancing 2 line.
           write print-line    from ws-r-heading-for-line-2
               after advancing 1 line.
           write print-line    from ws-r-heading-for-line-dash
               after advancing 1 line.
           write print-line    from ws-blank-lines
               after advancing 1 line.
      *
      * Line on a page
      *
       120-line-on-a-page.

           perform 130-calculate-totals.

           read input-file
               AT END move "t" to WS-EOF-FLAGS
               END-read.
      *
      * Calculating Totals
      *
       130-calculate-totals.

           if (transac-is-s) then
               add 1 to ws-total-for-s
           else if (transac-is-l) then
               add 1 to ws-total-l
           end-if.

           if (type-is-CA) then
               add 1 to ws-total-for-cash
           else if (type-is-CR) then
               add 1 to ws-total-for-credit
           else if (type-is-DB) then
               add 1 to ws-total-for-debit
           end-if.

           set indx-for-store to 1.

           perform 140-process-store varying
               indx-for-store from 1 by 1
               until indx-for-store > ws-total-number-of-stores.

           compute ws-tax-for-each-person ROUNDED =
               (input-rec-trans-amount * ws-tax-applicable) / 100.

           add ws-tax-for-each-person      to ws-tax-in-total.

           move input-rec-trans-code       to ws-trans-code.
           move input-rec-trans-amount     to ws-trans-amnt.
           move input-rec-pay-type         to ws-typ-of-paymnt.
           move input-rec-store-number     to ws-num-of-str.
           move input-rec-invoice-number   to ws-invc-num.
           move input-rec-sku-code         to ws-sku-c.
           move ws-tax-for-each-person     to ws-taxe.

           write print-line from ws-repo-info
               after advancing 1 line.
      *
      * Process Store
      *
       140-process-store.
           set index-const                 to indx-for-store.
           if( input-rec-store-number =
               ws-store-num-const(index-const)) then

               add input-rec-trans-amount
                   to ws-final-transaction-store(indx-for-store)

               move ws-final-transaction-store(indx-for-store)
                   to ws-total-transaction-initially

               if(ws-total-transaction-initially > 
                   ws-maximum-amount-of-trans) then
                   move ws-store-num-const(index-const)
                       to ws-max-no-of-transaction-store
                   move ws-total-transaction-initially
                       to ws-maximum-amount-of-trans
               end-if

               move ws-maximum-amount-of-trans
                   to ws-minimum-amount-of-trans

               if (ws-total-transaction-initially < 
                   ws-minimum-amount-of-trans) then
                   move ws-store-num-const(index-const)
                       to ws-min-no-of-transaction-store
                   move ws-total-transaction-initially
                       to ws-minimum-amount-of-trans
               end-if

           end-if.
      *
      * Print Footer
      *
       150-print-footer.

           perform 160-processing-variables.

           move ws-total-for-sl            to ws-tno-s-and-l-val
           write print-line from ws-tno-s-and-l
               after advancing 2 line.

           move ws-total-for-s             to ws-tno-s-val.
           write print-line from ws-tno-s
               after advancing 1 line.

           move ws-total-l             to ws-tno-l-val.
           write print-line from ws-tno-l
               after advancing 1 line.

           move ws-total-percentage-of-cash      to ws-tper-cash-val.
           move ws-total-percentage-of-credit    to ws-tper-credit-val.
           move ws-total-percentage-of-debit     to ws-tper-debit-val.
           write print-line from ws-payment-t-per
               after advancing 2 line.

           move ws-tax-in-total           to ws-tax-in-total-value.
           write print-line from ws-tax-in-total-owing
               after advancing 2 line.

           move ws-max-no-of-transaction-store to 
           ws-snum-w-h-sl-val.
           write print-line from ws-store-num-with-highest-sl
               after advancing 2 line.

           move ws-min-no-of-transaction-store  to 
           ws-snum-w-l-sl-val.
           write print-line from ws-store-num-with-lowest-sl
               after advancing 1 line.
      *
      * Processing Variables
      *
       160-processing-variables.

           compute ws-total-for-sl rounded = ws-total-for-s + 
               ws-total-l.

           compute ws-total-percentage-of-cash rounded =
             (ws-total-for-cash * 100) / ws-total-for-sl.

           compute ws-total-percentage-of-credit rounded =
             (ws-total-for-credit * 100) / ws-total-for-sl.

           compute ws-total-percentage-of-debit rounded =
             (ws-total-for-debit * 100) / ws-total-for-sl.

       end program program3.