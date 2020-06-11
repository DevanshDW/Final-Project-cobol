       identification division.
       program-id. program1.
       author. Smit Patel. Devansh Patel.
       date-written. 2020-04-01.
      * Description: Editing the input records to ensure validity and
      *  seperate valid and invalid records into respective output
      *  files.
      *
       environment division.
       input-output section.
       file-control.
      *
           select input-file
               assign to '../../../data/project3.dat'
               organization is line sequential.
      *
           select error-report-file
               assign to '../../../data/ErrorsReport.out'
               organization is line sequential.

            select invalid-data-file
               assign to '../../../data/InvalidData.dat'
               organization is line sequential.

            select valid-data-file
               assign to '../../../data/ValidData.dat'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd input-file
           data record is input-record
           record contains 36 characters.
      *
       01 input-record.
           05 input-record-trans-code          pic x(1).
               88 transac-is-s value "S".
               88 transac-is-r value "R".
               88 transac-is-l value "L".
           05 input-record-transac-amount      pic 9(5)v99.
           05 input-record-pay-type            pic x(2).
               88 type-is-CA value "CA".
               88 type-is-CR value "CR".
               88 type-is-DB value "DB".
           05 input-record-stor-number         pic x(2).
               88 store-is-01 value "01".
               88 store-is-02 value "02".
               88 store-is-03 value "03".
               88 store-is-04 value "04".
               88 store-is-05 value "05".
               88 store-is-12 value "12".
           05 input-record-invoice-number      pic x(9).
           05 define-invoice-number redefines
               input-record-invoice-number.
               10 invoice-alpha                pic x(2).
                   88 alpha-is-AA value "AA".
                   88 alpha-is-BB value "BB".
                   88 alpha-is-CC value "CC".
                   88 alpha-is-DD value "DD".
                   88 alpha-is-EE value "EE".
               10 invoice-dash                 pic x(1).
               10 invoice-number               pic 9(6).
           05 input-record-SKU-code            pic x(15).

      * Change characters for data line
       fd error-report-file
           data record is error-report-line
           record contains 73 characters.
       01 error-report-line                    pic x(73).

      * Change characters for data line
       fd invalid-data-file
           data record is invalid-data-line
           record contains 36 characters.
       01 invalid-data-line                    pic x(36).

      * Change characters for data line
       fd valid-data-file
           data record is valid-data-line
           record contains 36 characters.
       01 valid-data-line                      pic x(36).

       working-storage section.

       77 ws-eof-flag                          pic x value 'n'.
       77 ws-error-count                       pic 99 value 0.
       77 ws-record-number                     pic 9(3) value 0.
       77 ws-seperator                         pic x(59) value spaces.
       77 ws-valid-record                      pic 999 value 0.
       77 ws-invalid-record                    pic 999 value 0.
       77 ws-page-count                        pic 99 value 0.
       01 ws-invoice-alpha-char.
           05 ws-invoice-alpha-1               pic x.
           05 ws-invoice-alpha-2               pic x.

       01 ws-errors.
           05 ws-invalid-transaction-code      pic x(38) value
               "Transaction code is invalid.S,R,L only".
           05 ws-transac-not-numeric           pic x(36) value
               "Transaction amount should be numeric".
           05 ws-invalid-payment-type          pic x(46) value
               "Payment type must be either 'CA','CR' or 'DB'".
           05 ws-invlid-store-number           pic x(42) value
               "Store number should be from 01 to 05 or 12".
           05 ws-same-invoice-XX               pic x(36) value
               "First two characters cannot be same".
           05 ws-invalid-invoice-XX            pic x(44) value
               "First two characters must be A, B, C, D or E".
           05 ws-invoice-dash-dislocated       pic x(36) value
               " '-' should be at third position".
           05 ws-invalid-invoice-number        pic x(48) value
               "Invoice number should be between 900000 & 100000".
           05 ws-invoice-number-is-non-num     pic x(48) value
               "Invoice number must be numeric".
           05 ws-invalid-SKU-code              pic x(36) value
               "SKU code cannot be empty.".

       01 ws-data-tally-line.
           05 filler                           pic x(15) value 
               "Valid records: ".
           05 filler                           pic x(1).
           05 ws-prt-valid-records             pic 9(3).
           05 filler                           pic x(4).
           05 filler                           pic x(17) value
               "Invalid records: ".
           05 filler                           pic x(1).
           05 ws-prt-invalid-records           pic 99.
           05 filler                           pic x(30).

       01 ws-heading-line-1.
           05 filler                           pic x(24) value 
               "Program 1 - Error Report".
           05 filler                           pic x(24).
           05 filler                           pic x(10) value 
               "Smit Patel".

       01 ws-heading-line-2.
           05 ws-current-date                  pic 9(6).
           05 filler                           pic x(11).
           05 ws-current-time                  pic 9(7).
           05 filler                           pic x(21).
           05 filler                           pic x(13) value 
               "Devansh Patel".

       01 ws-record-with-error.
           05 filler                           pic x(9) value 
               "Record  :".
           05 ws-record-num-data               pic 9(3).
           05 filler                           pic x(10).
           05 ws-original-record               pic x(36).
           05 filler                           pic x(6).
      *
      * Procedure Division
      *
       procedure division.
       000-main.
      * Open files
           open input input-file,
             output error-report-file, invalid-data-file,
             valid-data-file.

           accept ws-current-date from date.
           accept ws-current-time from time.

           write error-report-line         from ws-heading-line-1.
           write error-report-line         from ws-heading-line-2.
      * Move spaces to ws-seperator.
           write error-report-line         from ws-seperator.
      * Read first record
           read input-file
               at end
                   move 'y'                to ws-eof-flag.

      * Perform 100 process logics
           perform 100-process-logic
               varying ws-page-count       from 1 by 1
                   until ws-eof-flag = 'y'.


           move ws-valid-record            to ws-prt-valid-records.
           move ws-invalid-record          to ws-prt-invalid-records.

           write error-report-line         from ws-data-tally-line.

           close input-file
               error-report-file, invalid-data-file,
               valid-data-file.

           stop run.
      *
      * Process logic
      *
       100-process-logic.
      * Perform validation for each lines
           perform 300-validation
               until ws-eof-flag = 'y'.
      *
      * Validation Procedure
      *
       300-validation.
           move
           "----------------------------------------------------------"
                                           to ws-seperator.
           move invoice-alpha              to ws-invoice-alpha-char.
      * Keep track of record number
           add 1                           to ws-record-number.
      * Check the transac code
           if (transac-is-s or transac-is-l or transac-is-r) then
      * Valid record
           else
               add 1                       to ws-error-count
               write error-report-line     from
                   ws-invalid-transaction-code
           end-if.
      * Check amount is numeric or not
           if (input-record-transac-amount is numeric) then
      * Valid Data
           else
               write error-report-line     from
                   ws-transac-not-numeric
               add 1                       to ws-error-count
           end-if.

      * Check the payment type
           if (type-is-CA or type-is-CR or type-is-DB) then
           else
               write error-report-line     from
                   ws-invalid-payment-type
               add 1                       to ws-error-count
           end-if.
      * Check the store  number
           if (store-is-01 or store-is-02 or store-is-03 or
               store-is-04 or store-is-05 or store-is-12) then
           else
               write error-report-line     from
                   ws-invlid-store-number
               add 1                       to ws-error-count
           end-if.
      * Check for invoice number
           if (alpha-is-AA or alpha-is-BB or alpha-is-CC or
               alpha-is-DD or alpha-is-EE) then
               write error-report-line     from
                 ws-same-invoice-XX
               add 1                       to ws-error-count
           end-if.
      * Check if invoice number is A B C D or E
           if((ws-invoice-alpha-1 = "A" or ws-invoice-alpha-1 = "B" or
               ws-invoice-alpha-1 = "C" or ws-invoice-alpha-1 = "D" or
               ws-invoice-alpha-1 = "E") and (
               ws-invoice-alpha-2 = "A" or ws-invoice-alpha-2 = "B" or
               ws-invoice-alpha-2 = "C" or ws-invoice-alpha-2 = "D" or
               ws-invoice-alpha-2 = "E")) then
      * Valid Data
           else
               write error-report-line     from ws-invalid-invoice-XX
               add 1 to ws-error-count
           end-if.
      * Check for dash
           if (invoice-dash = "-")         then
           else
               write error-report-line     from
                   ws-invoice-dash-dislocated
               add 1                       to ws-error-count
           end-if.
      * Check if invoice number is numeric
           if (invoice-number is numeric) then
           else
               write error-report-line     from
                   ws-invoice-number-is-non-num
               add 1                       to ws-error-count
           end-if
      * Check for number range
           if (invoice-number > 100000 and invoice-number <
             900000) then
           else
               write error-report-line     from
                   ws-invalid-invoice-number
               add 1                       to ws-error-count
           end-if
      * Check for 15 character SKU code
           if (input-record-SKU-code = space) then
               add 1                       to ws-error-count
               write error-report-line     from
                   ws-invalid-SKU-code
           else
           end-if.

      * If errors are 0, then move record to the valid data file
           if (ws-error-count = 0) then
               add 1                       to ws-valid-record
               write valid-data-line       from input-record
           else
               add 1                       to ws-invalid-record
               move ws-record-number       to ws-record-num-data
               move input-record           to ws-original-record
               write error-report-line     from ws-record-with-error
               write invalid-data-line     from input-record
               write error-report-line     from ws-seperator
                   after advancing 1 line
           end-if.

           move zeroes                     to ws-error-count.

      * Read next record from input file
           read input-file
               at end
                   move 'y'                to ws-eof-flag.

       end program program1.