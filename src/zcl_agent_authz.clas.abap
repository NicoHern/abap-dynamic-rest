CLASS zcl_agent_authz DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_allowed TYPE c LENGTH 1 VALUE 'A'.
    CONSTANTS c_denied  TYPE c LENGTH 1 VALUE 'D'.

    "! Route authorization, run before the handler object is created
    CLASS-METHODS check_route
      IMPORTING
        iv_auth_object     TYPE xuobject
        iv_auth_field      TYPE xufield
        iv_auth_value      TYPE xuval
        iv_auth_actvt      TYPE activ_auth
      RETURNING
        VALUE(rv_allowed)  TYPE abap_bool.

    "! Non-GET requests must be JSON, which a cross-site form cannot send
    CLASS-METHODS is_csrf_safe
      IMPORTING
        iv_http_method     TYPE string
        iv_content_type    TYPE string
      RETURNING
        VALUE(rv_safe)     TYPE abap_bool.

    "! Denylist plus the display authorization of the table's own group
    CLASS-METHODS check_table_read
      IMPORTING
        iv_table_name      TYPE dd02l-tabname
      RETURNING
        VALUE(rv_allowed)  TYPE abap_bool.

    "! True if the table, or any table behind a view, is denied
    CLASS-METHODS is_table_denied
      IMPORTING
        iv_table_name      TYPE dd02l-tabname
      RETURNING
        VALUE(rv_denied)   TYPE abap_bool.

    "! False if a dynamic WHERE clause contains a subquery
    CLASS-METHODS is_where_safe
      IMPORTING
        iv_where           TYPE string
      RETURNING
        VALUE(rv_safe)     TYPE abap_bool.

    "! Writes one audit row and commits it. Call before opening a cursor:
    "! the database commit closes open cursors.
    CLASS-METHODS audit
      IMPORTING
        io_request         TYPE REF TO if_http_request
        iv_decision        TYPE c
        iv_class_name      TYPE seoclsname OPTIONAL
        iv_method_name     TYPE seocpdname OPTIONAL
        iv_object_name     TYPE dd02l-tabname OPTIONAL
        iv_reason          TYPE string OPTIONAL.

  PRIVATE SECTION.
    TYPES tt_deny TYPE STANDARD TABLE OF zagent_deny WITH DEFAULT KEY.

    CLASS-METHODS name_is_denied
      IMPORTING
        iv_name            TYPE dd02l-tabname
        it_deny            TYPE tt_deny
      RETURNING
        VALUE(rv_denied)   TYPE abap_bool.

ENDCLASS.



CLASS zcl_agent_authz IMPLEMENTATION.

  METHOD check_route.
    rv_allowed = abap_false.

    IF iv_auth_object IS INITIAL.
      rv_allowed = abap_true.
      RETURN.
    ENDIF.

    IF iv_auth_field IS NOT INITIAL AND iv_auth_value IS INITIAL.
      " A field without a value is a configuration error: refuse
      RETURN.
    ENDIF.

    IF iv_auth_field IS NOT INITIAL AND iv_auth_actvt IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT iv_auth_object
        ID iv_auth_field FIELD iv_auth_value
        ID 'ACTVT' FIELD iv_auth_actvt.
    ELSEIF iv_auth_field IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT iv_auth_object
        ID iv_auth_field FIELD iv_auth_value.
    ELSEIF iv_auth_actvt IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT iv_auth_object
        ID 'ACTVT' FIELD iv_auth_actvt.
    ELSE.
      AUTHORITY-CHECK OBJECT iv_auth_object
        ID 'ACTVT' FIELD '03'.
    ENDIF.

    IF sy-subrc = 0.
      rv_allowed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_csrf_safe.
    DATA: lv_media TYPE string,
          lv_rest  TYPE string.

    rv_safe = abap_false.

    IF iv_http_method = 'GET' OR iv_http_method = 'HEAD'.
      rv_safe = abap_true.
      RETURN.
    ENDIF.

    " Compare the media type exactly: text/plain; x=application/json is a
    " content type a browser sends cross-site without asking
    SPLIT iv_content_type AT ';' INTO lv_media lv_rest.
    CONDENSE lv_media.
    TRANSLATE lv_media TO LOWER CASE.

    IF lv_media = 'application/json'.
      rv_safe = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD check_table_read.
    DATA lv_auth_group TYPE tddat-cclass.

    rv_allowed = abap_false.

    IF is_table_denied( iv_table_name ) = abap_true.
      RETURN.
    ENDIF.

    " Authorization group of the requested table; none assigned means &NC&
    SELECT SINGLE cclass FROM tddat
      INTO lv_auth_group
      WHERE tabname = iv_table_name.

    IF sy-subrc <> 0 OR lv_auth_group IS INITIAL.
      lv_auth_group = '&NC&'.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
      ID 'DICBERCLS' FIELD lv_auth_group
      ID 'ACTVT' FIELD '03'.

    IF sy-subrc = 0.
      rv_allowed = abap_true.
      RETURN.
    ENDIF.

    " Table-level authorization as the alternative to the group
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
      ID 'ACTVT' FIELD '03'
      ID 'TABLE' FIELD iv_table_name.

    IF sy-subrc = 0.
      rv_allowed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_table_denied.
    DATA: lt_deny   TYPE tt_deny,
          lt_open   TYPE STANDARD TABLE OF dd02l-tabname,
          lt_seen   TYPE STANDARD TABLE OF dd02l-tabname,
          lt_base   TYPE STANDARD TABLE OF dd02l-tabname,
          lv_name   TYPE dd02l-tabname,
          lv_sqltab TYPE dd02l-sqltab,
          lv_seen   TYPE i.

    rv_denied = abap_false.

    " Only names in DD02L can be checked. This refuses CDS entity names (use
    " the SQL view), physical cluster tables and names padded with blanks.
    SELECT SINGLE sqltab FROM dd02l
      INTO lv_sqltab
      WHERE tabname  = iv_table_name
        AND as4local = 'A'.
    IF sy-subrc <> 0.
      rv_denied = abap_true.
      RETURN.
    ENDIF.

    SELECT * FROM zagent_deny INTO TABLE lt_deny.

    " Walk through views so a denied table cannot be read through one
    APPEND iv_table_name TO lt_open.

    WHILE lt_open IS NOT INITIAL.
      READ TABLE lt_open INTO lv_name INDEX 1.
      DELETE lt_open INDEX 1.

      READ TABLE lt_seen TRANSPORTING NO FIELDS
        WITH KEY table_line = lv_name.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      APPEND lv_name TO lt_seen.

      DESCRIBE TABLE lt_seen LINES lv_seen.
      IF lv_seen > 200.
        " Too many tables behind this view to verify: refuse
        rv_denied = abap_true.
        RETURN.
      ENDIF.

      IF name_is_denied( iv_name = lv_name it_deny = lt_deny ) = abap_true.
        rv_denied = abap_true.
        RETURN.
      ENDIF.

      CLEAR lt_base.
      SELECT tabname FROM dd26s
        INTO TABLE lt_base
        WHERE viewname = lv_name
          AND as4local = 'A'.
      APPEND LINES OF lt_base TO lt_open.

      " A pool or cluster table is denied with its physical table
      CLEAR lv_sqltab.
      SELECT SINGLE sqltab FROM dd02l
        INTO lv_sqltab
        WHERE tabname  = lv_name
          AND as4local = 'A'.
      IF lv_sqltab IS NOT INITIAL.
        APPEND lv_sqltab TO lt_open.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD name_is_denied.
    DATA: ls_deny    TYPE zagent_deny,
          lv_name    TYPE string,
          lv_pattern TYPE string.

    rv_denied = abap_false.

    " Credentials, RFC destinations, keys and this framework's own tables
    CASE iv_name.
      WHEN 'USR02' OR 'USH02' OR 'USH02_ARC_TMP' OR 'USRPWDHISTORY'
        OR 'RFCDES' OR 'SSF_PSE_D'
        OR 'ZAGENT_DENY' OR 'ZAGENT_AUDIT' OR 'ZAGENT_ENDPOINTS'.
        rv_denied = abap_true.
        RETURN.
    ENDCASE.

    lv_name = iv_name.
    LOOP AT it_deny INTO ls_deny.
      lv_pattern = ls_deny-tabname.
      IF lv_name CP lv_pattern.
        rv_denied = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_where_safe.
    DATA: lv_clean TYPE string,
          lv_len   TYPE i,
          lv_pos   TYPE i,
          lv_char  TYPE c LENGTH 1,
          lv_quote TYPE c LENGTH 1.

    rv_safe = abap_false.

    lv_clean = iv_where.
    lv_len = strlen( iv_where ).

    " Blank out quoted literals so a value containing SELECT is still allowed
    WHILE lv_pos < lv_len.
      lv_char = iv_where+lv_pos(1).

      IF lv_quote IS INITIAL.
        IF lv_char = '''' OR lv_char = '`'.
          lv_quote = lv_char.
          REPLACE SECTION OFFSET lv_pos LENGTH 1 OF lv_clean WITH ` `.
        ENDIF.
      ELSE.
        IF lv_char = lv_quote.
          CLEAR lv_quote.
        ENDIF.
        REPLACE SECTION OFFSET lv_pos LENGTH 1 OF lv_clean WITH ` `.
      ENDIF.

      lv_pos = lv_pos + 1.
    ENDWHILE.

    IF lv_quote IS NOT INITIAL.
      " Unterminated literal: refuse rather than guess
      RETURN.
    ENDIF.

    TRANSLATE lv_clean TO UPPER CASE.

    FIND REGEX '\<SELECT\>' IN lv_clean.
    IF sy-subrc <> 0.
      rv_safe = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD audit.
    DATA: ls_audit TYPE zagent_audit,
          lv_value TYPE string.

    ls_audit-uname       = sy-uname.
    ls_audit-decision    = iv_decision.
    ls_audit-class_name  = iv_class_name.
    ls_audit-method_name = iv_method_name.
    ls_audit-object_name = iv_object_name.
    ls_audit-reason      = iv_reason.

    IF io_request IS BOUND.
      lv_value = io_request->get_header_field( '~path_info' ).
      ls_audit-endpoint_path = lv_value.
      lv_value = io_request->get_method( ).
      ls_audit-http_method = lv_value.
      lv_value = io_request->get_header_field( '~remote_addr' ).
      ls_audit-client_ip = lv_value.
    ENDIF.

    " One request writes several rows: SEQ_NO keeps them apart on a coarse clock
    GET TIME STAMP FIELD ls_audit-log_ts.
    DO 100 TIMES.
      ls_audit-seq_no = sy-index.
      INSERT zagent_audit FROM ls_audit.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'DB_COMMIT'.
  ENDMETHOD.

ENDCLASS.
