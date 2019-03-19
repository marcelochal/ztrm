CLASS zcl_fclm_apar_hadi_sample DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb .
    INTERFACES if_fclm_hadi_apar .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_fclm_apar_hadi_sample IMPLEMENTATION.

  METHOD if_fclm_hadi_apar~flow_adjust BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT USING
   skb1 vfclmbamaclvw.
    et_flows =  select
                origin_trans_qualifier,
                fi_document_number,
                company_code,
                fi_document_line_item,
                planning_group,
                planning_level,
                fi_fiscal_year,
                fi_account,
                document_date,
                base_currency,
                c.currency,
                cash_discount_percent1,
                cash_discount_days1,
                cash_discount_percent2,
                cash_discount_days2,
                days_due,
                baseline_date,
                payment_method,
                payment_block,
                assignment,
*                case when bank_account_id = '' then coalesce(d.acc_id, '') ELSE bank_account_id END as bank_account_id,
*                case when bank_account_id = null then coalesce(d.acc_id, '') ELSE bank_account_id END as bank_account_id,
                ifnull(bank_account_id, coalesce(d.acc_id, '')) as bank_account_id,
                assigned_company_code,
                certainty_level,
                flow_type,
                origin_document_id_rl,
                cleared,
                origin_transaction_id_rl,
                origin_document_id,
                origin_flow_id,
                base_amount,
                AMOUNT,
                origin_transaction_id,
                transaction_date,
                fi_purchse_document_number,
                fi_purchse_line_item,
                fi_sequential_number,
                business_area,
                house_bank,
                house_bank_account,
                cost_center,
                customer_number,
                vendor_number,
                liquidity_item,
                material,
                profit_center,
                project,
                c.segment,
                trading_partner,
                contract_number,
                contract_type
                from (  select
                        origin_trans_qualifier,
                        fi_document_number,
                        company_code,
                        fi_document_line_item,
                        planning_group,
                        planning_level,
                        fi_fiscal_year,
                        fi_account,
                        document_date,
                        base_currency,
                        currency,
                        cash_discount_percent1,
                        cash_discount_days1,
                        cash_discount_percent2,
                        cash_discount_days2,
                        days_due,
                        baseline_date,
                        payment_method,
                        payment_block,
                        assignment,
                        bank_account_id,
                        assigned_company_code,
                        certainty_level,
                        flow_type,
                        origin_document_id_rl,
                        cleared,
                        origin_transaction_id_rl,
                        origin_document_id,
                        origin_flow_id,
                        base_amount,
                        amount,
                        origin_transaction_id,
                        transaction_date,
                        fi_purchse_document_number,
                        fi_purchse_line_item,
                        fi_sequential_number,
                        business_area,
                        case when house_bank = '' then coalesce(b.hbkid, '') ELSE house_bank END as house_bank,
                        CASE when house_bank_account = '' then coalesce(b.hktid, '') ELSE house_bank_account END as house_bank_account,
                        cost_center,
                        customer_number,
                        vendor_number,
                        liquidity_item,
                        material,
                        profit_center,
                        project,
                        SEGMENT,
                        trading_partner,
                        contract_number,
                        contract_type
                        from :it_flows as a
                        inner join skb1 as b
                        ON  b.mandt         = :iv_mandt
                        and a.company_code  =  b.bukrs
                        and a.fi_account    = b.saknr) as c
                left outer join vfclmbamaclvw as d
                ON  d.mandt         = :iv_mandt
                and d.linkage_bukrs = c.company_code
                and d.linkage_hbkid = c.house_bank
                and d.linkage_hktid = c.house_bank_account
                and c.transaction_date >= d.linkage_valid_from
                and c.transaction_date <= d.linkage_valid_to;

  endmethod.


ENDCLASS.

