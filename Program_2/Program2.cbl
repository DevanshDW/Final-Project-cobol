       identification division.
       program-id. program2.
       author. Smit Patel. Devansh Patel.
       date-written. 2020-04-01.
      * Description: This program help in data splitting
      * and counting recourds. This program also output data files.

       environment division.
       input-output section.
       file-control.

           select input-file
               assign to "../../../data/ValidData.dat"
               organization is line sequential.

           select cct-report
               assign to "../../../data/counts_control_totals.out"
               organization is line sequential.

           select sl-record-file
               assign to "../../../data/s_l_records.dat"
               organization is line sequential.

           select rr_file
               assign to "../../../data/returns_records.dat"
               organization is line sequential.
      *
       data division.
       file section.
       fd input-file
           data record is input-record
           record contains 36 characters.
      *
       01 input-record.
           05 filler                           pic x(1).
               88 transac-is-s value "S".
               88 transac-is-r value "R".
               88 transac-is-l value "L".
           05 input-rec-trans-amount           pic 9(5)v99.
           05 filler                           pic x(2).
               88 type-is-CA value "CA".
               88 type-is-CR value "CR".
               88 type-is-DB value "DB".
           05 input-rec-store-number           pic x(2).
           05 input-rec-invoice-number         pic x(9).
           05 input-rec-SKU-code               pic x(15).

      *
       fd cct-report
           data record is cct-report-line
           record contains 132 characters.

       01 cct-report-line                      pic x(132).
      *
       fd sl-record-file
           data record is sl-record-line
           record contains 36 characters.

       01 sl-record-line                       pic x(36).
      *
       fd rr_file
           data record is cct-report-linec
           record contains 36 characters.

       01 cct-report-linec                     pic x(36).
      *
      * Working Storage Section
      *
       working-storage section.
      *
       01 name-line-1.
           05 filler                           pic x(40) value 
               "FINAL PROJECT - PROGRAM 2".
           05 filler                           pic x(57).
           05 filler                           pic x(10) value 
               "Group  6: ".
           05 filler                           pic x(15) value 
               "Smit Patel".
           05 filler                           pic x(16).
      *
       01 name-line-2.
           05 ws-sys-date                      pic x(8).
           05 filler                           pic x(8).
           05 ws-sys-time                      pic x(7).
           05 filler                           pic x(84).
           05 filler                           pic x(30) values 
               "Devansh Patel".
           05 filler                           pic x(16).
      *
       01 ws-heading-1.
           05 filler                           pic x(46).
           05 filler                           pic x(30) value
               "COUNTS & CONTROL TOTALS REPORT".
      *
       01 ws-dash-line-1.
           05 filler                           pic x(46).
           05 filler                           pic x(30) value
               "------------------------------".
      *
       01 ws-dash-line-2.
           05 filler                           pic x(54).
           05 filler                           pic x(14) value 
               "--------------".
      *
       01 ws-total-S-L-heading-line.
           05 filler                           pic x(2) value spaces.
           05 filler                           pic x(12) value spaces.
           05 filler                           pic x(2) value spaces.
           05 filler                           pic x(8) value
               "  Sales".
           05 filler                           pic x(4) value spaces.
           05 filler                           pic x(8) value 
               " Layaway".
           05 filler                           pic x(4) value spaces.
           05 filler                           pic x(8) value 
               "   S & L".
           05 filler                           pic x(4) value spaces.
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
           05 filler                           pic x(10) value spaces.
      *
       01 ws-total-S-L-heading-line-dash.
           05 filler                           pic x(2) value spaces.
           05 filler                           pic x(12) value spaces.
           05 filler                           pic x(2) value spaces.
           05 filler                           pic x(8) value
               "  -----".
           05 filler                           pic x(4) value spaces.
           05 filler                           pic x(8) value 
               "--------".
           05 filler                           pic x(4) value spaces.
           05 filler                           pic x(8) value 
               "   -----".
           05 filler                           pic x(4) value spaces.
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "---------".
           05 filler                           pic x(10) value spaces.
      *
       01 ws-total-sl-line1.
           05 filler                           pic x(14) value 
               "Total Number".
           05 ws-total-line-s-num              pic z,zzz,zz9.
           05 filler                           pic x(4) value spaces.
           05 ws-total-line-l-num              pic z,zzz,zz9.
           05 filler                           pic x(3) value spaces.
           05 ws-total-line-sl-num             pic z,zzz,zz9.
           05 filler                           pic x(2) value spaces.
           05 ws-total-line-sl-store-nums.
               10 ws-total-line-sl-store-num
                   occurs 6 times.
                   15 ws-s-l-store-num         pic zz,zzz,zz9.
                   15 filler                   pic x(2) value spaces.
           05 filler                           pic x(10).
      *
       01 ws-total-S-L-line2.
           05 filler                           pic x(13) value 
               "Total Amount".
           05 filler                           pic x(1) value spaces.
           05 ws-total-line-s-amt              pic zz,zz9.99.
           05 filler                           pic x(4) value spaces.
           05 ws-total-line-l-amt              pic zz,zz9.99.
           05 filler                           pic x(2) value spaces.
           05 ws-total-line-sl-amt             pic zzz,zz9.99.
           05 filler                           pic x(2) value spaces.
           05 ws-total-line-sl-store-amts.
               10 ws-total-line-sl-store-amt
                   occurs 6 times.
                   15 ws-s-l-store-amt         pic zzz,zz9.99.
                   15 filler                   pic x(2) value spaces.
           05 filler                           pic x(10).
      *
       01 ws-payment-type-line1.
           05 filler                           pic x(16) value 
               "Payment Type:".
           05 filler                           pic x(10) value spaces.
           05 filler                           pic x(10) value
               "    CA".
           05 filler                           pic x(10) value spaces.
           05 filler                           pic x(10) value
               "    CR".
           05 filler                           pic x(10) value spaces.
           05 filler                           pic x(10) value
               "    DB".
           05 filler                           pic x(54) value spaces.
      *
       01 ws-payment-type-line2.
           05 filler                           pic x(16) value 
               "% of Transaction".
           05 filler                           pic x(7) value spaces.
           05 ws-percent-ca                    pic zzz,zz9.99.
           05 filler                           pic x(10) value "%".
           05 ws-percent-cr                    pic zzz,zz9.99.
           05 filler                           pic x(10) value "%".
           05 ws-percent-db                    pic zzz,zz9.99.
           05 filler                           pic x(10) value "%".
           05 filler                           pic x(47).
      *
       01 ws-total-R-line.
           05 filler                           pic x(54).
           05 filler                           pic x(14) value 
               "RETURNS REPORT".
      *
       01 ws-total-r-heading-line.
           05 filler                           pic x(15) value spaces.
           05 filler                           pic x(7) value
               " Return".
           05 filler                           pic x(5) value spaces.
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
           05 filler                           pic x(33) value spaces.
      *
       01 ws-total-r-heading-line-dash.
           05 filler                           pic x(15) value spaces.
           05 filler                           pic x(7) value
               " ------".
           05 filler                           pic x(5) value spaces.
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "--------".
           05 filler                           pic x(12) value 
               "---------".
           05 filler                           pic x(33) value spaces.
      *
       01 ws-total-R-line1.
           05 filler                           pic x(12) value 
               "Total Number".
           05 filler                           pic x(1) value spaces.
           05 ws-total-line-r-num              pic z,zzz,zz9.
           05 filler                           pic x(3) value spaces.
           05 ws-total-line-r-store-nums.
               10 ws-total-line-r-store-num
                   occurs 6 times.
                   15 ws-retrun-store-num      pic zz,zzz,zz9.
                   15 filler                   pic x(2) value spaces.
           05 filler                           pic x(33) value spaces.

      *
       01 ws-total-R-line2.
           05 filler                           pic x(12) value 
               "Total Amount".
           05 ws-total-line-r-amount           pic zzz,zz9.99.
           05 filler                           pic x(3) value spaces.
           05 ws-total-line-r-store-amounts.
               10 ws-total-line-r-store-amount
                   occurs 6 times.
                   15 ws-return-store-amount   pic zzz,zz9.99.
                   15 filler                   pic x(2) value spaces.
           05 filler                           pic x(33).



       01 ws-total-line.
           05 filler                           pic x(21) value 
               "Grand Total Amount :".
           05 ws-total-line-gra-total          pic $$$,$$9.99.
           05 filler                           pic x(80).

      *
       01 ws-boolean-cnst.
           05 ws-true-cnst                     pic x value "Y".
           05 ws-false-cnst                    pic x value "N".
      *
       01 ws-code-cnst.
           05 ws-number-of-stores              pic 99 value 6.

       01 ws-data.
           05 filler                           pic 99 value 1.
           05 filler                           pic 99 value 2.
           05 filler                           pic 99 value 3.
           05 filler                           pic 99 value 4.
           05 filler                           pic 99 value 5.
           05 filler                           pic 99 value 12.

       01 ws-store-no-cnst redefines ws-data
           occurs 6 times pic 99.

      *
       77 ws-s-amt-total                       pic 9(7)v99 value 0.
       77 ws-l-amt-total                       pic 9(7)v99 value 0.
       77 ws-r-amt-total                       pic 9(7)v99 value 0.
       77 ws-s-l-amt-total                     pic 9(7)v99 value 0.
       77 ws-grand-total                       pic 9(7)v99 value 0.
       77 ws-ca-percent                        pic 99v99 value 0.
       77 ws-cr-percent                        pic 99v99 value 0.
       77 ws-db-percent                        pic 99v99 value 0.
       77 ws-s-l-cnt                           pic 999 value 0.
       77 ws-records-cnt                       pic 999 value 0.
       77 ws-s-cnt                             pic 999 value 0.
       77 ws-r-cnt                             pic 999 value 0.
       77 ws-l-cnt                             pic 999 value 0.
       77 ws-ca-cnt                            pic 999 value 0.
       77 ws-cr-cnt                            pic 999 value 0.
       77 ws-db-cnt                            pic 999 value 0.
       77 ws-store-sub                         pic 9 value 0.

      *
       01 ws-totals.
           05 ws-sl-store-nums.
               10 ws-sl-store-num              pic 99 value 0
                   occurs 6 times.
           05 ws-sl-stores-amt.
               10 ws-sl-store-amt              pic 9(5)v99 value 0 
                   occurs 6 times.
           05 ws-r-stores-num.
               10 ws-r-store-num               pic 99 value 0
                   occurs 6 times.
           05 ws-r-store-amts.
               10 ws-r-store-amt               pic 9(5)v99 value 0 
                   occurs 6 times.
      *
       01 ws-flags.
           05 ws-eof-flag                      pic x value 'n'.
               88 eof value 'y'.

       procedure division.
      *
       000-main.
      *
           move ws-false-cnst to ws-eof-flag.
      *
           open input input-file.
           open output cct-report,
               sl-record-file,
               rr_file.
      *
           accept ws-sys-date from date.
           accept ws-sys-time from time.

           read input-file
               at end
                   set eof to true.

           perform 100-output-headings.
           perform 200-process-input
               until eof.

           perform 300-output-summary.
      *
           close input-file,
               cct-report,
               sl-record-file,
               rr_file.
      *
           goback.

       100-output-headings.
      * advance page only when this is the second page and beyond

           write cct-report-line from name-line-1.
           write cct-report-line from name-line-2.
           write cct-report-line from ws-heading-1
               after advancing 1 line.


       200-process-input.
           add 1 to ws-records-cnt.

           if (transac-is-s or transac-is-l) then
      * S & L Check
               if (transac-is-s)
                   add 1 to ws-s-cnt
                   add input-rec-trans-amount
                       to ws-s-amt-total
                   write sl-record-line from input-record
               else
                   add 1 to ws-l-cnt
                   add input-rec-trans-amount
                       to ws-l-amt-total
                   write sl-record-line from input-record
               end-if

               if (type-is-CA) then
                   add 1               to ws-ca-cnt
               end-if
               if (type-is-CR) then
                   add 1               to ws-cr-cnt
               end-if
               if (type-is-DB) then
                   add 1               to ws-db-cnt
               end-if

               perform varying ws-store-sub
                 from 1 by 1
                 until ws-store-sub > ws-number-of-stores
                   if (input-rec-store-number = ws-store-no-cnst(
                     ws-store-sub)) then
                       add 1
                           to ws-sl-store-num(ws-store-sub)
                       add input-rec-trans-amount
                           to ws-sl-store-amt(ws-store-sub)
                   end-if
               end-perform
           else
      * R check
               if (transac-is-r)
                   add 1 to ws-r-cnt
                   add input-rec-trans-amount
                     to ws-r-amt-total
                   write cct-report-linec from input-record
               end-if

               perform varying ws-store-sub
                 from 1 by 1
                 until ws-store-sub > ws-number-of-stores
                   if (input-rec-store-number = ws-store-no-cnst(
                     ws-store-sub)) then
                       add 1 to
                         ws-r-store-num(ws-store-sub)
                       add input-rec-trans-amount
                         to
                         ws-r-store-amt(ws-store-sub)
                   end-if
               end-perform
           end-if.

           read input-file
               at end
                   set eof to true.
      *
      * Print Output Summary
      *
       300-output-summary.
      *
           compute ws-s-l-amt-total rounded =
             ws-s-amt-total + ws-l-amt-total.
           compute ws-s-l-cnt = ws-s-cnt + ws-l-cnt.
           compute ws-grand-total =
             ws-s-l-amt-total - ws-r-amt-total.

           compute ws-ca-percent rounded =
             (ws-ca-cnt / ws-s-l-cnt) * 100.
           compute ws-cr-percent rounded =
             (ws-cr-cnt / ws-s-l-cnt) * 100.
           compute ws-db-percent rounded =
             (ws-db-cnt / ws-s-l-cnt) * 100.

           perform varying ws-store-sub
               from 1 by 1
               until ws-store-sub > ws-number-of-stores
                   move ws-sl-store-num(ws-store-sub)
                       to ws-s-l-store-num(ws-store-sub)
                   move ws-sl-store-amt(ws-store-sub)
                       to ws-s-l-store-amt(ws-store-sub)
                   move ws-r-store-num(ws-store-sub)
                       to ws-retrun-store-num(ws-store-sub)
                   move ws-r-store-amt(ws-store-sub)
                       to ws-return-store-amount(ws-store-sub)
           end-perform.

           move ws-s-cnt               to ws-total-line-s-num.
           move ws-s-amt-total         to ws-total-line-s-amt.

           move ws-l-cnt               to ws-total-line-l-num.
           move ws-l-amt-total         to ws-total-line-l-amt.

           move ws-s-l-cnt             to ws-total-line-sl-num.
           move ws-s-l-amt-total       to ws-total-line-sl-amt.

           move ws-ca-percent          to ws-percent-ca.
           move ws-cr-percent          to ws-percent-cr.
           move ws-db-percent          to ws-percent-db.

           move ws-r-cnt               to ws-total-line-r-num.
           move ws-r-amt-total         to ws-total-line-r-amount.

           move ws-grand-total         to ws-total-line-gra-total.


           write cct-report-line from ws-dash-line-1
               after advancing 1 lines.
           write cct-report-line from ws-total-S-L-heading-line
               after advancing 2 lines.
           write cct-report-line from ws-total-S-L-heading-line-dash
               after advancing 1 lines.
           write cct-report-line from ws-total-sl-line1
               after advancing 1 line.
           write cct-report-line from ws-total-S-L-line2
               after advancing 1 line.

           write cct-report-line from ws-payment-type-line1
               after advancing 2 lines.
           write cct-report-line from ws-payment-type-line2
               after advancing 1 line.

           write cct-report-line from ws-total-R-line
               after advancing 2 lines.
           write cct-report-line from ws-dash-line-2
               after advancing 1 lines.
           write cct-report-line from ws-total-r-heading-line
               after advancing 2 lines.
           write cct-report-line from ws-total-r-heading-line-dash
               after advancing 1 lines.
           write cct-report-line from ws-total-R-line1
               after advancing 1 line.
           write cct-report-line from ws-total-R-line2
               after advancing 1 line.

           write cct-report-line from ws-total-line
               after advancing 3 lines.

      *
       end program program2.