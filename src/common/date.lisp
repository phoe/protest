;;;; src/common/date.lisp

(in-package #:protest/common)

(define-protocol date
    (:documentation "The DATE protocol describes a timestamp object, ~
representing a point in time. These objects are immutable, have microsecond ~
precision and can be compared to other timestamp objects, converted to string ~
and from string representations, precisely Unix timestamps.
\
The time units, available in comparison functions, are :YEAR :MONTH :DAY :HOUR ~
:MINUTE :SECOND :MICROSECOND. If the key argument UNIT is supplied with one of ~
these values, the comparison takes into account that unit's granularity. The ~
default unit is :MICROSECOND.
\
Examples:
* 29th July 2017 and 30th July 2017 will not be DATE= under :UNIT :DAY, but ~
will be equal under :UNIT :MONTH.
* 31st July 2017 and 1st August 2017 will not be DATE= under :UNIT :DAY or
:UNIT :MONTH, but will be equal under :UNIT :YEAR."
     :tags (:date)
     :dependencies (serializable)
     :export t)
  (:class date (serializable) ())
  "A date object. See protocol DATE for details."
  (:function date-timestamp ((date date)) integer)
  "Converts a date object to a Unix timestamp."
  (:function timestamp-date-using-class
             ((class class) (timestamp integer)) date)
  "Converts a Unix timestamp to a date object of provided class."
  (:function date-ustimestamp ((date date)) integer)
  "Converts a date object to a Unix timestamp with microsecond precision."
  (:function ustimestamp-date-using-class
             ((class class) (nstimestamp integer)) date)
  "Converts a Unix timestamp with microsecond precision to a date object of ~
provided class."
  (:function date= ((date-1 date) (date-2 date) &key) t)
  "Returns true iff the two dates are equal under the provided granularity ~
unit."
  (:function date/= ((date-1 date) (date-2 date) &key) t)
  "Returns true iff the two dates are not equal under the provided granularity ~
unit."
  (:function date> ((date-1 date) (date-2 date) &key) t)
  "Returns true iff the first date is greater than the other under the ~
provided granularity unit."
  (:function date>= ((date-1 date) (date-2 date) &key) t)
  "Returns true iff the first date is not less than the other under the ~
provided granularity unit."
  (:function date< ((date-1 date) (date-2 date) &key) t)
  "Returns true iff the first date is less than the other under the provided ~
granularity unit."
  (:function date<= ((date-1 date) (date-2 date) &key) t)
  "Returns true iff the first date is not greater than the other under the ~
provided granularity unit."
  (:function date-min ((date date) &rest other-dates) date)
  "Returns the oldest date from all provided dates."
  (:function date-max ((date date) &rest other-dates) date)
  "Returns the newest date from all provided dates."
  (:function now-using-class ((class class)) date)
  "Returns the date object of class CLASS that corresponds to the current ~
time, relative to the call of this function."
  (:variable *date-granularity-units* t
             '(:year :month :day :hour :minute :second :microsecond))
  "List of all available granularity units, in decreasing size.")

(execute-protocol date)
