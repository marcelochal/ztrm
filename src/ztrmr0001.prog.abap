*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Marcelo Alvares                                 *
* Res. Funcional...: Bernardo Torres                                 *
* Módulo...........: TRM                                             *
* Programa.........: ZTRMR0001                                       *
* Transação........: ZTRMR0001                                       *
* Tipo de Programa.: REPORT                                          *
* Request     .....: TBDK903099                                      *
* Objetivo.........: Carga para fundos de investimentos              *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    1.00 | 06/06/18  | Marcelo Alvares     |   Versão Inicial       *
**********************************************************************
REPORT ztrmr0001
  MESSAGE-ID zpmcarga
  NO STANDARD PAGE HEADING LINE-SIZE 72.

TYPE-POOLS: icon.

TABLES: sscrfields.

*&---------------------------------------------------------------------*
*& Class definition Messages
*&---------------------------------------------------------------------*
CLASS lcl_messages DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      initialize,
      store
        IMPORTING VALUE(i_bapiret2_t) TYPE bapiret2_t,
      show
        IMPORTING VALUE(i_sel) TYPE slis_selfield,
      progress_indicator
        IMPORTING
          VALUE(iv_test)      TYPE xtest
          VALUE(iv_processed) TYPE syst-tabix
          VALUE(iv_total)     TYPE syst-tabix.

ENDCLASS.

**********************************************************************
* Declaração de Tipos
**********************************************************************
TYPES:

  BEGIN OF ty_e_file_upload,
    company_code        TYPE bapi_ftr_create-company_code,
    portfolio           TYPE bapi_ftr_create-portfolio,
    security_id         TYPE bapi_ftr_create_security-security_id,
    transaction_type    TYPE bapi_ftr_create-transaction_type,
    partner             TYPE bapi_ftr_create-partner,
    security_account    TYPE bapi_ftr_create_security-security_account,
    valuation_class     TYPE bapi_ftr_create-valuation_class,
    position_value_date TYPE bapi_ftr_create_security-position_value_date,
    calculate_date      TYPE bapi_ftr_create_security-calculate_date,
    payment_date        TYPE bapi_ftr_create_security-payment_date,
    number_units        TYPE bapi_ftr_create_security-number_units,
    price               TYPE bapi_ftr_create_security-price,
    house_bank          TYPE bapi_ftr_paydet_create-house_bank,
    account_id          TYPE bapi_ftr_paydet_create-account_id,
  END OF   ty_e_file_upload,

  BEGIN OF ty_e_result,
    status      TYPE tp_icon,
    transaction TYPE bapi2042-transaction,
  END OF   ty_e_result.

TYPES: BEGIN OF ty_e_alv.
    INCLUDE TYPE ty_e_result.
    INCLUDE TYPE ty_e_file_upload.
TYPES: END OF   ty_e_alv,

ty_ti_alv TYPE STANDARD TABLE OF ty_e_alv.

**********************************************************************
* Tabelas internas
**********************************************************************
DATA:
  it_alv    TYPE TABLE OF ty_e_alv.   "Resultado a ser exibido

**********************************************************************
* Constantes
**********************************************************************
CONSTANTS:
  cc_parameter_id TYPE memoryid     VALUE 'ZTRM_FILE',
  cc_msg_error    TYPE smesg-msgty  VALUE 'E'.  " Type of message (E).

**********************************************************************
* Tela de Seleção
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS:
  p_file TYPE rlgrap-filename OBLIGATORY
    DEFAULT 'C:\*.xls' MEMORY ID cc_parameter_id.

SELECTION-SCREEN:
    FUNCTION KEY 1,
    SKIP,
    BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  p_header AS CHECKBOX DEFAULT abap_true.

SELECTION-SCREEN:
     END OF BLOCK b11,
     END OF BLOCK b1.

*******************************************
* INITIALIZATION
*******************************************
INITIALIZATION.
  PERFORM fs_set_sscrtexts_dynsel3.

* Pega dados em memoria para o campo
  GET PARAMETER ID cc_parameter_id FIELD p_file.

*******************************************
* AT SELECTION-SCREEN
*******************************************
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN'FC01'.
      PERFORM exporta_modelo.
  ENDCASE.

**********************************************************************
* Critica tela de selecao
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM file_f4_process
    USING p_file.

**********************************************************************
* Lógica Principal
**********************************************************************
START-OF-SELECTION.

  PERFORM file_validate_path  USING p_file.

  PERFORM file_import_excel.

  PERFORM call_bapi_ftr_create USING abap_true.

  PERFORM alv_exibe USING abap_false.

*&---------------------------------------------------------------------*
*&      Form  FILE_F4_PROCESS
*&---------------------------------------------------------------------*
*      -->p_file  Caminho do arquivo
*----------------------------------------------------------------------*
FORM file_f4_process USING p_file.

* seleção de arquivo
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'p_file'
    IMPORTING
      file_name     = p_file.

  SET PARAMETER ID cc_parameter_id FIELD p_file.

ENDFORM.                    "F4_FILE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  fs_set_sscrtexts
*&---------------------------------------------------------------------*
FORM fs_set_sscrtexts_dynsel3.
  DATA l_text LIKE smp_dyntxt.

  MOVE:
    icon_export          TO l_text-icon_id,
    'Baixar modelo'(010) TO l_text-text,                    "#EC *
    'Baixar modelo'(010) TO l_text-icon_text.

  sscrfields-functxt_01 = l_text.

ENDFORM.                               " SET_SSCRTEXTS

*&---------------------------------------------------------------------*
*&      Form  FILE_VALIDATE_PATH
*&---------------------------------------------------------------------*
*      -->P_FILE_PATH  Caminho do arquivo
*----------------------------------------------------------------------*
FORM file_validate_path USING p_file_path.

  DATA :
    lv_dir      TYPE string,
    lv_file     TYPE string,
    lv_filename TYPE string.

  lv_filename = p_file_path.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file_path
    IMPORTING
      stripped_name = lv_file   "file name
      file_path     = lv_dir    "directory path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF cl_gui_frontend_services=>directory_exist( directory = lv_dir ) IS INITIAL.
    MESSAGE ID 'C$' TYPE cc_msg_error NUMBER '155' "Não foi possível abrir o file &1&3 (&2)
      WITH p_file_path .
  ENDIF.

  " check file existence
  IF cl_gui_frontend_services=>file_exist( file = lv_filename ) IS INITIAL.
    MESSAGE ID 'FES' TYPE cc_msg_error NUMBER '000'. "O file não existe
  ENDIF.

ENDFORM.                    "file_validate_path
*&---------------------------------------------------------------------*
*& Form FILE_IMPORT_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM file_import_excel.

  DATA :
    it_raw         TYPE truxs_t_text_data,
    it_file_upload TYPE STANDARD TABLE OF ty_e_file_upload.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = p_header
      i_tab_raw_data       = it_raw                 " WORK TABLE
      i_filename           = p_file
    TABLES
      i_tab_converted_data = it_file_upload
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING it_file_upload TO it_alv.

*** Início - 19/10/2018 - Implementação de Authority-Check Projeto AGIR
  PERFORM f_authority_check TABLES it_alv
                            USING  '01'. "Criar
*** Fim    - 19/10/2018 - Implementação de Authority-Check Projeto AGIR


ENDFORM.      "file_import_excel

*&---------------------------------------------------------------------*
*& Form ALV_EXIBE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM alv_exibe USING p_test TYPE xtest.

  DATA: it_fieldcat   TYPE slis_t_fieldcat_alv,
        wl_fieldcat   TYPE LINE OF slis_t_fieldcat_alv,
        it_s_fcat     TYPE TABLE OF lvc_s_fcat,
*        it_events     TYPE slis_t_event,
        lv_title      TYPE lvc_title,
        lv_gui_status TYPE slis_formname,
        lv_no_out(1)  TYPE c.

  PERFORM alv_create_fieldcatalog CHANGING it_fieldcat.

*Verifica o tipo de resultado a ser exibido
  IF p_test IS INITIAL.
    lv_title = 'Dados importados e resultado do processamento de teste'(003).
    lv_gui_status = 'ALV_SET_STATUS_01'.
    lv_no_out = abap_true.

  ELSE.
    lv_title = 'Resultado do processamento da carga'(004).
    lv_gui_status = space.
    lv_no_out = space.
  ENDIF.

* Inibe o campo de transação
  READ TABLE it_fieldcat INTO wl_fieldcat INDEX 2.
  wl_fieldcat-no_out = lv_no_out.
  wl_fieldcat-no_zero = abap_true.
  MODIFY it_fieldcat FROM wl_fieldcat INDEX 2.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = syst-cprog
      i_callback_pf_status_set = lv_gui_status
      i_callback_user_command  = 'USER_COMMAND'
      i_grid_title             = lv_title
      it_fieldcat              = it_fieldcat
*     it_events                = it_events
    TABLES
      t_outtab                 = it_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form  alv_create_fieldcatalog
*&---------------------------------------------------------------------*
* Cria catalogo de campos a partir de uma tabela interna
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  Catalogo de campos
*----------------------------------------------------------------------*
FORM  alv_create_fieldcatalog
       CHANGING  pt_fieldcat  TYPE slis_t_fieldcat_alv.

  DATA:
    lr_tabdescr TYPE REF TO cl_abap_structdescr,
    lr_data     TYPE REF TO data,
    lt_dfies    TYPE ddfields.

  CLEAR:
    pt_fieldcat.

  CREATE DATA lr_data TYPE ty_e_alv.

  lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
  lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

  MOVE-CORRESPONDING lt_dfies TO pt_fieldcat.

  LOOP AT pt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

*Campo Status definir como icone
    IF sy-tabix EQ 1.
      <fs_fieldcat>-icon = abap_true.
    ENDIF.
*Campo Transação definir como one clique
    IF sy-tabix EQ 2.
      <fs_fieldcat>-no_zero = abap_true.
      <fs_fieldcat>-hotspot = abap_true.
    ENDIF.

* Retira zeros a esqueda do BP Partner
    IF sy-tabix EQ 7.
      <fs_fieldcat>-no_zero = abap_true.
    ENDIF.

*  Define os campos como centralizado
    <fs_fieldcat>-just = 'C'.

  ENDLOOP.


ENDFORM.                    "alv_create_fieldcatalog
*&---------------------------------------------------------------------*
*& Form CALL_BADI_FTR_CREATE
*&---------------------------------------------------------------------*
*& Chamada para a BAPI
*&---------------------------------------------------------------------*
*& -->  p_test        parametro para execução de teste
*&---------------------------------------------------------------------*
FORM call_bapi_ftr_create USING p_test TYPE xtest.

  DATA:
    ls_security            TYPE bapi_ftr_create_security,
    ls_generalcontractdata TYPE bapi_ftr_create,
    ls_paymentdetail       TYPE bapi_ftr_paydet_create,
    lv_flow_type           TYPE bapi_ftr_paydet_detail-flow_type,
    it_bapi_return         TYPE TABLE OF bapiret2,
    it_return              TYPE TABLE OF bapiret2,
    lv_tabix               TYPE syst-tabix.

  LOOP  AT it_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    CLEAR:
        it_bapi_return, ls_security, ls_generalcontractdata.

    CALL METHOD lcl_messages=>progress_indicator
      EXPORTING
        iv_test      = p_test
        iv_processed = lv_tabix
        iv_total     = lines( it_alv ).

    lv_tabix = sy-tabix.

    MOVE-CORRESPONDING:
       <fs_alv> TO ls_security,
       <fs_alv> TO ls_generalcontractdata.

    ls_generalcontractdata-partner = |{ ls_generalcontractdata-partner ALPHA = IN }|.

*executa a BADI para realziar os lançamentos
    CALL FUNCTION 'BAPI_FTR_SECURITY_CREATE'
      EXPORTING
        security             = ls_security
        generalcontractdata  = ls_generalcontractdata
        testrun              = p_test
      IMPORTING
        financialtransaction = <fs_alv>-transaction
      TABLES
        return               = it_bapi_return.

    " Faz o link entre o retorno e a tabela do ALV
    LOOP AT it_bapi_return ASSIGNING FIELD-SYMBOL(<fs_bapi_return>).
      <fs_bapi_return>-row = lv_tabix.
    ENDLOOP.

    APPEND LINES OF it_bapi_return TO it_return.

*Executa o commit se caso não for execução de teste
    IF p_test IS INITIAL.

      PERFORM call_transaction_commit.

      IF <fs_alv>-transaction_type EQ '100'.
        ls_paymentdetail-direction = '-'.
      ELSEIF <fs_alv>-transaction_type EQ '200'.
        ls_paymentdetail-direction = '+'.
      ENDIF.

      ls_paymentdetail-payment_currency = 'BRL'.
      MOVE-CORRESPONDING <fs_alv> TO ls_paymentdetail.

      CLEAR: it_bapi_return.

      CALL FUNCTION 'BAPI_FTR_PAYDET_CREATE'
        EXPORTING
          companycodein          = <fs_alv>-company_code        " Company Code of Financial Transaction
          financialtransactionin = <fs_alv>-transaction         " Financial Transaction Number
          directionin            = ls_paymentdetail-direction   " FTR: BAPI Structure for Creating Payment Details
          paymentcurrencyin      = ls_paymentdetail-payment_currency  " Currency Key
          effectivedatein        = <fs_alv>-payment_date        " Payment Details Effective From
          flowtypein             = lv_flow_type                 " Flow Type
          paymentdetail          = ls_paymentdetail             " FTR: BAPI Structure for Creating Payment Details
        TABLES
          return                 = it_bapi_return.              " Return Parameters

      PERFORM call_transaction_commit.

      LOOP AT it_bapi_return ASSIGNING <fs_bapi_return>.
        <fs_bapi_return>-row = lv_tabix.
      ENDLOOP.

      APPEND LINES OF it_bapi_return TO it_return.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.

    "Verifica se existe alguma mensagem de erro no retorno da BAPI
    READ TABLE it_return WITH KEY type = 'E' row = lv_tabix TRANSPORTING NO FIELDS.

    IF sy-subrc IS INITIAL. "Se achou algum erro
      <fs_alv>-status = icon_led_red.
    ELSEIF p_test EQ abap_true.
      <fs_alv>-status = icon_led_green. "Verde na execução de teste
    ELSE.
      <fs_alv>-status = icon_okay.      "Ok na execução
    ENDIF.

  ENDLOOP.

  CALL METHOD lcl_messages=>store
    EXPORTING
      i_bapiret2_t = it_return.


ENDFORM.

FORM call_transaction_commit.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.        " Use of Command `COMMIT AND WAIT`

ENDFORM.
*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
*& Form utilizado dinamicamente pela ALV
*&---------------------------------------------------------------------*
*& -->  p_ucomm        parametro com o user command
*& <--  p_sel          Dados da alv
*&---------------------------------------------------------------------*
FORM user_command USING p_ucomm LIKE sy-ucomm
                        p_sel   TYPE slis_selfield.

  CONSTANTS:
    cc_double_click(4)  TYPE c VALUE '&IC1',
    cc_executar_lanc(9) TYPE c VALUE '&FTR_EXEC'.

  CASE p_ucomm.
    WHEN cc_double_click . "Duplo clique na linha


      IF p_sel-fieldname EQ 'TRANSACTION' "CALL TRANSACTION TS06
        AND p_sel-value IS NOT INITIAL.

*       Seleciona a empresa da transação
        READ TABLE it_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX p_sel-tabindex.

*        UNPACK p_sel-value TO lv_transaction.

        CALL FUNCTION 'FWTR_SEC_TRANS_MAINTAIN'
          EXPORTING
            i_company_code             = <fs_alv>-company_code
            i_transaction_number       = <fs_alv>-transaction
            i_transactiontype          = '1'
          EXCEPTIONS
            company_code_initial       = 1
            transaction_number_initial = 2
            OTHERS                     = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ELSE.

*     exibe STORAGE de mensagens armazenadas.
        CALL METHOD lcl_messages=>show( p_sel ).

      ENDIF.

    WHEN cc_executar_lanc. "Executa lançamento

      PERFORM call_bapi_ftr_create USING abap_false.
      PERFORM alv_exibe USING abap_true. "Exibe resultados
      "Sai da transação e retorna para a mesma, evita o empilhamento.
      LEAVE TO CURRENT TRANSACTION.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM ALV_SET_STATUS_01                                        *
*---------------------------------------------------------------------*
*& -->  p_extab          Parametro utilizado pela função
*                         REUSE_ALV_GRID_DISPLAY
*&--------------------------------------------------------------------*
FORM alv_set_status_01 USING p_extab TYPE slis_t_extab.

  SET PF-STATUS 'STATUS_01' EXCLUDING p_extab.

ENDFORM.                    "ALV_SET_STATUS_01

FORM exporta_modelo.

  DATA:
    lo_table  TYPE REF TO cl_salv_table,
    lt_model  TYPE STANDARD TABLE OF ty_e_file_upload,
    lx_xml    TYPE xstring,
    vl_return TYPE c.

  CONSTANTS:
    c_default_extension TYPE string VALUE 'xlsx',
    c_default_file_name TYPE string VALUE ' modelo.xlsx',
    c_default_mask      TYPE string VALUE 'Excel (*.xlsx)|*.xlsx' ##NO_TEXT.

  TRY .
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = lt_model ).
    CATCH cx_root.

  ENDTRY.

  lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX

  CALL FUNCTION 'XML_EXPORT_DIALOG'
    EXPORTING
      i_xml                      = lx_xml
      i_default_extension        = c_default_extension
*     i_initial_directory        = lcl_file=>get_download_path
      i_initial_directory        = 'C:\Temp\'
      i_default_file_name        = syst-tcode && c_default_file_name
      i_mask                     = c_default_mask
    EXCEPTIONS
      application_not_executable = 1
      OTHERS                     = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*& Class (Implementation) cl_messages
*&---------------------------------------------------------------------*
CLASS lcl_messages IMPLEMENTATION.

  METHOD initialize.

*    Inicializa message store.
    CALL FUNCTION 'MESSAGES_ACTIVE'
      EXCEPTIONS
        not_active = 1
        OTHERS     = 2.
    IF sy-subrc EQ 1.
      CALL FUNCTION 'MESSAGES_INITIALIZE'
        EXPORTING
*         collect_and_send      = space
          i_store_duplicates    = abap_true
          i_no_duplicate_count  = 500
*         check_on_commit       = 'X'
          i_allow_foreign_reset = abap_false
*         i_reset_line          = 'X'
        EXCEPTIONS
          log_not_active        = 1
          wrong_identification  = 2
          OTHERS                = 3.
    ENDIF.

  ENDMETHOD.

  METHOD show.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        line_from           = i_sel-tabindex  " Only show messages with longer reference line
        line_to             = i_sel-tabindex  " Only show messages with shorter reference line
        object              = sy-repi2        " Object for title in message dialog box
        batch_list_type     = 'B'             " J = job log / L = in spool list / B = both
        show_linno          = abap_true       " Also show line numbers
        show_linno_text     = 'Item'          " Column header for row
        show_linno_text_len = '3'             " Column width for row in display
        i_use_grid          = abap_true       " Use ALV Grid for Display; Otherwise Classic ALV
      EXCEPTIONS
        inconsistent_range  = 1               " LINE_TO is shorter than LINE_FROM
        no_messages         = 2               " No messages in required interval
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD store .

    CALL METHOD lcl_messages=>initialize.

    LOOP AT i_bapiret2_t ASSIGNING FIELD-SYMBOL(<fs_bapiret2>).

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          exception_if_not_active = abap_true                " X = exception not_active is initialized if
          arbgb                   = <fs_bapiret2>-id         " Message ID
          msgty                   = <fs_bapiret2>-type       " Type of message (I, S, W, E, A)
          msgv1                   = <fs_bapiret2>-message_v1 " First variable parameter of message
          msgv2                   = <fs_bapiret2>-message_v2 " Second variable parameter of message
          msgv3                   = <fs_bapiret2>-message_v3 " Third variable parameter of message
          msgv4                   = <fs_bapiret2>-message_v4 " Fourth variable parameter of message
          txtnr                   = <fs_bapiret2>-number     " Message Number
          zeile                   = <fs_bapiret2>-row        " Reference line (if it exists)
        EXCEPTIONS
          message_type_not_valid  = 1                     " Type of message not I, S, W, E or A
          not_active              = 2                     " Collection of messages not activated
          OTHERS                  = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*& PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      --> iv_text        Texto a ser utilizado na mensagem
*&      --> iv_processed   Valor sendo processado
*&      --> iv_total       Total do valores a serem processados
*&---------------------------------------------------------------------*
  METHOD progress_indicator.

    DATA lv_text TYPE string.

    IF iv_test EQ abap_true.
      lv_text = 'Executando lançamentos de teste'(007).
    ELSE.
      lv_text = 'Executando lançamentos efetivos'(008).
    ENDIF.

    CALL METHOD cl_progress_indicator=>progress_indicate
      EXPORTING
        i_text               = | { lv_text } [{ iv_processed } de { iv_total }] |
        i_processed          = iv_processed
        i_total              = iv_total
        i_output_immediately = abap_true.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Form F_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_authority_check TABLES p_it_alv    TYPE ty_ti_alv
                       USING  p_atividade TYPE zag_campo.

*** Constantes
  CONSTANTS:
    lc_t_hm_buk TYPE xuobject  VALUE 'T_HM_BUK',
    lc_x        TYPE char1     VALUE 'X'.

*** Variáveis
  DATA:
    lv_erro     TYPE char1,
    lv_objeto   TYPE xuobject,
    lv_campo1   TYPE zag_campo,
    lv_campo2   TYPE zag_campo,
    lv_mensagem TYPE string.

*** Estruturas
  DATA ls_dados  TYPE ty_e_alv.

*** Tabelas internas
  DATA lt_dados TYPE ty_ti_alv.

  CLEAR lv_erro.
  lt_dados[] = p_it_alv[].
  SORT lt_dados BY company_code.
  DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING company_code.

*** Verifica autorização para cada empresa
  LOOP AT lt_dados INTO ls_dados.

*** Preenche variáveis
    lv_objeto = lc_t_hm_buk.
    lv_campo1 = ls_dados-company_code.
    lv_campo2 = p_atividade.

*** Executa função padrão para verificação de autorizações
    CALL FUNCTION 'ZAG_F_AUTHORITY'
      EXPORTING
        i_tcode    = sy-tcode
        i_xuobject = lv_objeto
        i_campo1   = lv_campo1
        i_campo2   = lv_campo2
      IMPORTING
        e_mensagem = lv_mensagem.

*** Caso o usuário não tenha autorização, exibe mensagem e interrompe o processamento
    IF lv_mensagem IS NOT INITIAL.
      MESSAGE i000(zfi0) WITH lv_mensagem.
      lv_erro = lc_x.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF lv_erro IS NOT INITIAL.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
