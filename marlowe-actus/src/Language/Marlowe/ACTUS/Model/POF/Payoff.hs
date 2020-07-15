{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.Payoff where

import Data.Time ( Day )
import Language.Marlowe.ACTUS.Definitions.ContractState
    ( ContractStatePoly(..),
      ContractState )
import Language.Marlowe.ACTUS.Definitions.BusinessEvents
    ( EventType(..), RiskFactors(..))
import Language.Marlowe.ACTUS.Model.POF.PayoffModel
    ( _POF_IED_PAM,
      _POF_MD_PAM,
      _POF_PP_PAM,
      _POF_PY_PAM,
      _POF_FP_PAM,
      _POF_PRD_PAM,
      _POF_TD_PAM,
      _POF_IP_PAM )
import Language.Marlowe.ACTUS.Definitions.ContractTerms
    ( ContractTerms(..), ContractType(LAM, PAM) )
import Language.Marlowe.ACTUS.Ops ( YearFractionOps(_y) )



payoff :: EventType -> RiskFactors -> ContractTerms -> ContractState -> Day -> Double
payoff ev RiskFactors{..} ContractTerms{..} ContractStatePoly {..} t = 
    let
        y_sd_t = _y _DCC sd t _MD
    in case contractType of
        PAM ->
            case ev of
                IED -> _POF_IED_PAM o_rf_CURS _CNTRL _NT _PDIED
                MD  -> _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
                PP  -> _POF_PP_PAM o_rf_CURS pp_payoff
                PY  -> _POF_PY_PAM _PYTP o_rf_CURS o_rf_RRMO _PYRT _cPYRT _CNTRL nt ipnr y_sd_t
                FP  -> _POF_FP_PAM _FEB _FER o_rf_CURS _CNTRL nt fac y_sd_t
                PRD -> _POF_PRD_PAM o_rf_CURS _CNTRL _PPRD ipac ipnr nt y_sd_t
                TD  -> _POF_TD_PAM o_rf_CURS _CNTRL _PTD ipac ipnr nt y_sd_t
                IP  -> _POF_IP_PAM o_rf_CURS isc ipac ipnr nt y_sd_t
                _             -> 0.0
        LAM -> undefined


