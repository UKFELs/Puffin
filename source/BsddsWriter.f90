! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

MODULE SddsWriter
USE paratype
USE FileType
USE CIOWrapper
IMPLICIT NONE


CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteVersion(version, &
                            tFileType)
CHARACTER(*), INTENT(IN)       :: version
TYPE(cFileType), INTENT(INOUT)  :: tFileType
LOGICAL :: qOKL

IF (tFileType%qFormatted) Then 
   WRITE(UNIT=tFileType%iUnit,FMT='(A,A)') 'SDDS',version
ELSE
   Call C_WriteString(tFileType%zFileName,'SDDS' // Trim(version), qOKL, qNewLine=.TRUE.)
   Call C_WriteString(tFileType%zFileName,'!# little-endian', qOKL, qNewLine=.TRUE.)
END IF

END SUBROUTINE SddsWriteVersion

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteDescription(text,contents, &
                            tFileType)
CHARACTER(*), INTENT(IN),  OPTIONAL :: text,contents
TYPE(cFileType),      INTENT(INOUT)         :: tFileType
CHARACTER(LEN = 1000)  ::zString
LOGICAL :: qOKL
IF (tFileType%qFormatted) Then 

   WRITE(UNIT=tFileType%iUnit,FMT='(A12)')           '&description'

   IF (PRESENT(text)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A9,A1,A,A1)')  '  text = ','"',text,'"'
   ELSE
     WRITE(UNIT=tFileType%iUnit,FMT='(A)')           '  text = "Unassigned text field"'
   END IF

   IF (PRESENT(contents)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A13,A1,A,A1)') '  contents = ','"',contents,'"'
   ELSE
     WRITE(UNIT=tFileType%iUnit,FMT='(A)')           '  contents = "Unassigned contents field"'
   END IF

   WRITE(UNIT=tFileType%iUnit,FMT='(A4)')            '&end'
ELSE
   zString = '&description'
    IF (PRESENT(text)) THEN
     zString = Trim(zString) // ' text=' // Trim(text) // ','
   ELSE
     zString = Trim(zString) // ' text="Unassigned text field"' // ','
   END IF

   IF (PRESENT(contents)) THEN
     zString = Trim(zString) // ' contents=' // Trim(contents) // ','
   ELSE
     zString = Trim(zString) // ' contents="Unassigned contents field"' // ','
   END IF  
   zString = Trim(zString) // '&end'

   Call C_WriteString(tFileType%zFileName, Trim(zString), qOKL, qNewLine=.TRUE.)
    
END IF
END SUBROUTINE SddsWriteDescription

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteParameter(name,typ,symbol,units, &
                              description,FormatString, &
                              FixedValue_sh,FixedValue_ln, &
                              FixedValue_fl,FixedValue_db, &
                              FixedValue_ch,FixedValue_st, &
                              tFileType)
CHARACTER(*),        INTENT(IN)            :: name,typ
CHARACTER(*),        INTENT(IN),  OPTIONAL :: symbol,units,description, &
                                              FormatString,FixedValue_st
CHARACTER,           INTENT(IN),  OPTIONAL :: FixedValue_ch
INTEGER(KIND=short), INTENT(IN),  OPTIONAL :: FixedValue_sh
INTEGER(KIND=long),  INTENT(IN),  OPTIONAL :: FixedValue_ln
REAL(KIND=float),    INTENT(IN),  OPTIONAL :: FixedValue_fl
REAL(KIND=double),   INTENT(IN),  OPTIONAL :: FixedValue_db
TYPE(cFileType),      INTENT(INOUT)         :: tFileType


CHARACTER(LEN = 1000)  ::zString
CHARACTER(LEN = 32)    ::zChar
LOGICAL :: qOKL
IF (tFileType%qFormatted) Then 

   WRITE(UNIT=tFileType%iUnit,FMT='(A10)')           '&parameter'
   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')          '  name = ',name
   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')          '  type = ',typ
   IF (PRESENT(symbol)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A11,A1,A,A1)') '  symbol = ','"',symbol,'"'
   END IF
   IF (PRESENT(units)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A10,A)')       '  units = ',units
   END IF
   IF (PRESENT(description)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A16,A1,A,A1)') '  description = ','"',description,'"'
   END IF
   IF (PRESENT(FormatString)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A18,A1,A,A1)') '  format_string = ','"',FormatString,'"'
   END IF
   IF (PRESENT(FixedValue_sh)) THEN
     IF(typ.EQ.'short') THEN
       WRITE(UNIT=tFileType%iUnit,FMT='(A16,I6)')    '  fixed_value = ',FixedValue_sh
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_ln)) THEN
     IF(typ.EQ.'long') THEN
       WRITE(UNIT=tFileType%iUnit,FMT='(A16,I11)')   '  fixed_value = ',FixedValue_ln
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_fl)) THEN
     IF(typ.EQ.'float') THEN
       WRITE(UNIT=tFileType%iUnit,FMT='(A16,ES13.6E2)')     '  fixed_value = ',FixedValue_fl
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_db)) THEN
     IF(typ.EQ.'double') THEN
       IF (ABS(LOG10(FixedValue_db)).LT.REAL(100.0_WP)) THEN
         WRITE(UNIT=tFileType%iUnit,FMT='(A16,ES21.14E2)')     '  fixed_value = ',FixedValue_db
       ELSE
         WRITE(UNIT=tFileType%iUnit,FMT='(A16,ES22.14E3)')     '  fixed_value = ',FixedValue_db
       END IF
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_ch)) THEN
     IF(typ.EQ.'character') THEN
       WRITE(UNIT=tFileType%iUnit,FMT='(A16,3A1)')   '  fixed_value = ','"',FixedValue_ch,'"'
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_st)) THEN
     IF(typ.EQ.'string') THEN
       WRITE(UNIT=tFileType%iUnit,FMT='(A16,A1,A,A1)')     '  fixed_value = ','"',FixedValue_st,'"'
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF

   WRITE(UNIT=tFileType%iUnit,FMT='(A4)')            '&end'
ELSE
   zString = '&parameter'
   zString = Trim(zString) // ' name=' // TRIM(name) // ','
   zString = Trim(zString) // ' type=' // TRIM(typ) // ','
   IF (PRESENT(symbol)) THEN
     zString = Trim(zString) // ' symbol=' // TRIM(symbol) // ','
   END IF
   IF (PRESENT(units)) THEN
     zString = Trim(zString) // ' units=' // TRIM(units) // ','
   END IF
   IF (PRESENT(description)) THEN
     zString = Trim(zString) // ' description=' // TRIM(description) // ','
   END IF
   IF (PRESENT(FormatString)) THEN
     zString = Trim(zString) // ' format_string='  // TRIM(FormatString) // ','
   END IF
   IF (PRESENT(FixedValue_sh)) THEN
     IF(typ.EQ.'short') THEN
       write(zChar,*) FixedValue_sh
       zString = Trim(zString) // ' fixed_value=' // TRIM(ADJUSTL(zChar)) // ','
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_ln)) THEN
     IF(typ.EQ.'long') THEN
       write(zChar,*) FixedValue_ln
       zString = Trim(zString) //    ' fixed_value=' // TRIM(ADJUSTL(zChar)) // ','
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_fl)) THEN
     IF(typ.EQ.'float') THEN
       write(zChar,*) FixedValue_fl
       zString = Trim(zString) // ' fixed_value=' // TRIM(ADJUSTL(zChar)) // ','
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_db)) THEN
     IF(typ.EQ.'double') THEN
       write(zChar,*) FixedValue_db
       zString = Trim(zString) // ' fixed_value=' // TRIM(ADJUSTL(zChar)) // ','
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_ch)) THEN
     IF(typ.EQ.'character') THEN
       zString = Trim(zString) //    ' fixed_value=' // TRIM(FixedValue_ch) // ','
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF
   IF (PRESENT(FixedValue_st)) THEN
     IF(typ.EQ.'string') THEN
       zString = Trim(zString) //      ' fixed_value=' // TRIM(FixedValue_st)  // ','
     ELSE
       STOP "Type and Fixedvalue mismatch in SddsWriteParameter"
     END IF
   END IF 
   zString = Trim(zString) // '&end'
   
   Call C_WriteString(tFileType%zFileName, Trim(zString), qOKL, qNewLine=.TRUE.)
END IF
END SUBROUTINE SddsWriteParameter

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteColumn(name,typ,symbol,units, &
                           description,FormatString,FieldLength, &
                           tFileType)
CHARACTER(*),       INTENT(IN)            :: name,typ
CHARACTER(*),       INTENT(IN),  OPTIONAL :: symbol,units,description, &
                                             FormatString
INTEGER(KIND=long), INTENT(IN),  OPTIONAL :: FieldLength
TYPE(cFileType),     INTENT(INOUT)         :: tFileType

CHARACTER*1000  ::zString
LOGICAL :: qOKL
CHARACTER* 32    ::zChar
IF (tFileType%qFormatted) Then 

   WRITE(UNIT=tFileType%iUnit,FMT='(A7)')            '&column'

   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')          '  name = ',name
   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')          '  type = ',typ
   IF (PRESENT(symbol)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A11,A1,A,A1)') '  symbol = ','"',symbol,'"'
   END IF
   IF (PRESENT(units)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A10,A)')       '  units = ',units
   END IF
   IF (PRESENT(description)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A16,A1,A,A1)') '  description = ','"',description,'"'
   END IF
   IF (PRESENT(FormatString)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A18,A1,A,A1)') '  format_string = ','"',FormatString,'"'
   END IF
   IF (PRESENT(FieldLength)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A17,I11)')     '  field_length = ',FieldLength
   END IF
   
   WRITE(UNIT=tFileType%iUnit,FMT='(A4)')            '&end'
ELSE
   zString = '&column'
   zString = Trim(zString) // ' name=' // Trim(name) // ','
   zString = Trim(zString) // ' type=' // Trim(typ) // ','
   IF (PRESENT(symbol)) THEN
     zString = Trim(zString) // ' symbol='  // Trim(symbol) // ','
   END IF
   IF (PRESENT(units)) THEN
     zString = Trim(zString) // ' units=' // trim(units) // ','
   END IF
   IF (PRESENT(description)) THEN
     zString = Trim(zString) // ' description=' // trim(description) // ','
   END IF
   IF (PRESENT(FormatString)) THEN
     zString = Trim(zString) // ' format_string=' // Trim(FormatString) // ','
   END IF
   IF (PRESENT(FieldLength)) THEN
     write(zChar,*) FieldLength
     zString = Trim(zString) // ' field_length=' // TRIM(ADJUSTL(zChar)) // ','
   END IF
  
   zString = Trim(zString) // '&end'
   Call C_WriteString(tFileType%zFileName, Trim(zString), qOKL, qNewLine=.TRUE.)
END IF

END SUBROUTINE SddsWriteColumn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteInclude(FName, &
                           tFileType)
CHARACTER(*),   INTENT(IN)     :: FName
TYPE(cFileType), INTENT(INOUT)  :: tFileType

CHARACTER(LEN = 1000)  ::zString
LOGICAL :: qOKL
IF (tFileType%qFormatted) Then 

   WRITE(UNIT=tFileType%iUnit,FMT='(A8)')            '&include'
   WRITE(UNIT=tFileType%iUnit,FMT='(A13,A1,A,A1)')   '  filename = ','"',FName,'"'
   WRITE(UNIT=tFileType%iUnit,FMT='(A4)')            '&end'
ELSE
   zString = '&include'
   zString = Trim(zString) // ' filename=' // Trim(FName) // ','
   zString = Trim(zString) // '&end'
   
   Call C_WriteString(tFileType%zFileName, Trim(zString), qOKL, qNewLine=.TRUE.)
END IF

END SUBROUTINE SddsWriteInclude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteDataMode(mode,lines_per_row, &
                             no_row_counts,additional_header_lines, &
                             tFileType)
CHARACTER(*),       INTENT(IN)            :: mode
INTEGER(KIND=long), INTENT(IN),  OPTIONAL :: lines_per_row,no_row_counts, &
                                             additional_header_lines
TYPE(cFileType)    , INTENT(INOUT)         :: tFileType

CHARACTER(LEN = 1000)  ::zString
LOGICAL :: qOKL
CHARACTER(LEN = 32)    ::zChar
IF (tFileType%qFormatted) Then 
   WRITE(UNIT=tFileType%iUnit,FMT='(A5)')                   '&data'
   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')                 '  mode = ',mode
   IF (PRESENT(lines_per_row)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A18,I11)')            '  lines_per_row = ',lines_per_row
   END IF
   IF  (PRESENT(no_row_counts)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A18,I11)')            '  no_row_counts = ',no_row_counts
   END IF
   IF (PRESENT(additional_header_lines)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A28,I11)')            '  additional_header_lines = ',additional_header_lines
   END IF
   WRITE(UNIT=tFileType%iUnit,FMT='(A4)')                   '&end'
ELSE
   zString = '&data'
   zString = Trim(zString) // ' mode=' // Trim(mode) // ','
   IF (PRESENT(lines_per_row)) THEN
     write(zChar,*) lines_per_row
     zString = Trim(zString) // ' lines_per_row=' // TRIM(ADJUSTL(zChar)) // ','
   END IF
   IF  (PRESENT(no_row_counts)) THEN
     write(zChar,*) no_row_counts
     zString = Trim(zString) // ' no_row_counts=' // TRIM(ADJUSTL(zChar)) // ','
   END IF
   IF (PRESENT(additional_header_lines)) THEN
     write(zChar,*) additional_header_lines
     zString = Trim(zString) // ' additional_header_lines=' // TRIM(ADJUSTL(zChar)) // ','
   END IF

   zString = Trim(zString) // '&end'
   
   Call C_WriteString(tFileType%zFileName, Trim(zString), qOKL, qNewLine=.TRUE.)
END IF
END SUBROUTINE SddsWriteDataMode

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWriteArray(name,typ,symbol,units, &
                          description,FormatString,GroupName, &
                          FieldLength,dimensions, &
                          tFileType)
CHARACTER(*),       INTENT(IN)            :: name,typ
CHARACTER(*),       INTENT(IN),  OPTIONAL :: symbol,units,description, &
                                             FormatString,GroupName
INTEGER(KIND=long), INTENT(IN),  OPTIONAL :: FieldLength,dimensions
TYPE(cFileType),     INTENT(INOUT)         :: tFileType 
CHARACTER(LEN = 1000)  ::zString
LOGICAL :: qOKL
CHARACTER(LEN = 32)    ::zChar
IF (tFileType%qFormatted) Then 
   WRITE(UNIT=tFileType%iUnit,FMT='(A6)')            '&array'

   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')          '  name = ',name
   WRITE(UNIT=tFileType%iUnit,FMT='(A9,A)')          '  type = ',typ
   IF (PRESENT(symbol)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A11,A1,A,A1)') '  symbol = ','"',symbol,'"'
   END IF
   IF (PRESENT(units)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A10,A)')       '  units = ',units
   END IF
   IF (PRESENT(description)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A16,A1,A,A1)') '  description = ','"',description,'"'
   END IF
   IF (PRESENT(FormatString)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A18,A1,A,A1)') '  format_string = ','"',FormatString,'"'
   END IF
   IF (PRESENT(GroupName)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A15,A1,A,A1)') '  group_name = ','"',GroupName,'"'
   END IF
   IF (PRESENT(FieldLength)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A17,I11)')     '  field_length = ',FieldLength
   END IF
   IF (PRESENT(dimensions)) THEN
     WRITE(UNIT=tFileType%iUnit,FMT='(A15,I11)')     '  dimensions = ',dimensions
   END IF

   WRITE(UNIT=tFileType%iUnit,FMT='(A4)')            '&end'
ELSE
   zString = '&array'

   zString = Trim(zString) // ' name = ' // TRIM(name) // ','
   zString = Trim(zString) // ' type = ' // TRIM(typ) // ','
   IF (PRESENT(symbol)) THEN
     zString = Trim(zString) // ' symbol = ' // TRIM(symbol) // ','
   END IF
   IF (PRESENT(units)) THEN
     zString = Trim(zString) // ' units = ' // TRIM(units) // ','
   END IF
   IF (PRESENT(description)) THEN
     zString = Trim(zString) // ' description = ' // TRIM(description) // ','
   END IF
   IF (PRESENT(FormatString)) THEN
     zString = Trim(zString) // ' format_string = ' // TRIM(FormatString) // ','
   END IF
   IF (PRESENT(GroupName)) THEN
     zString = Trim(zString) // ' group_name = ' // TRIM(GroupName) // ','
   END IF
   IF (PRESENT(FieldLength)) THEN
     write(zChar,*) FieldLength
     zString = Trim(zString) // ' field_length = ' // TRIM(ADJUSTL(zChar)) // ','
   END IF
   IF (PRESENT(dimensions)) THEN
     write(zChar,*)dimensions
     zString = Trim(zString) // ' dimensions = ' // TRIM(ADJUSTL(zChar)) // ','
   END IF
   zString = Trim(zString) // '&end'
   
   Call C_WriteString(tFileType%zFileName, Trim(zString), qOKL, qNewLine=.TRUE.)

END IF
END SUBROUTINE SddsWriteArray

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE SddsWritePage(iPage, &
                         tFileType)
INTEGER(KIND=long), INTENT(IN)            :: iPage
TYPE(cFileType),    INTENT(INOUT)         :: tFileType

IF (tFileType%qFormatted) THEN
   WRITE (UNIT=tFileType%iUnit,FMT='(A,I10)') '! Page # ', iPage
END IF
END SUBROUTINE SddsWritePage

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


END MODULE SddsWriter

