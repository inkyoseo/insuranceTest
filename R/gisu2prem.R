#' Title Convert/calculate Premiums of Gisu-table based on informations called base
#'
#' @param 기수표 Gisu-table
#' @param exp expense table
#'
#' @return Premiums
#' @export
#'
#' @examples
gisu2prem   <- function(base, exp, ET) {

col_gisu2prem <- 기수표 %>% data.frame() %>% head(1) %>% 
        select(seq:t, -seq, -t) %>% colnames()
     기수표 %>% group_by(seq) %>% summarise(across(col_gisu2prem, 
        first), MxMx_s = list(Mx[t == 0] - Mx[t == 보험기간]), 
        NNxNNx_s = list(NNx[t == 0] - NNx[t == pmin(보험기간, 
            20)]),
              보험가입금액 = min(보험가입금액), 
              DxDx_r = list(Dxr[t == 0] - Dxr[t == 납입기간]), 
              MxMx_r = list(Mxr[t == 0] - Mxr[t == 보험기간]), 
              NxNx_r = list(Nxr[t == 0] - Nxr[t == 납입기간]), 
              FPI = list(Nx[t == 납입기간] - Nx[t == 보험기간]), 
              FPII = list(NNx[t == 납입기간] - NNx[t == 보험기간]), 
              NxNx = list(Nx[t == 납입기간] - Nx[t == 납입기간]), 
              MxMx = list(Mx[t == 0] - Mx[t == 보험기간]), 
              DxDx = list(Dx[t == 0] - Dx[t == 보험기간]), 
              DDxDDx = list(DDx[t == 0] - DDx[t == 납입기간]), 
              NNxNNx = list(NNx[t == 0] - NNx[t == 납입기간]), 
              Dx = list(Dx[t == 0]), 
              NNxNNx_납후 = list(NNx[t == 납입기간] - NNx[t == 보험기간]), 
        ) %>%
merge(ET) %>% 
rowwise() %>% 
mutate(NNxNNx_연납 = (NNxNNx - 
        (1 - 1)/(2 * 1) * DDxDDx), NNxNNx_6월납 = (NNxNNx - (2 - 
        1)/(2 * 2) * DDxDDx), NNxNNx_3월납 = (NNxNNx - (4 - 1)/(2 * 
        4) * DDxDDx), NNxNNx_2월납 = (NNxNNx - (6 - 1)/(2 * 6) * 
        DDxDDx), NNxNNx_월납 = (NNxNNx - (12 - 1)/(2 * 12) * 
        DDxDDx), 
        기준연납순보험료 = as.numeric(ifelse(납입기간 == 
        0, 0, MxMx_s/NNxNNx_s)), 순보험료_연납 = ifelse(납입기간 == 
        0, MxMx/Dx, MxMx/NNxNNx_연납/1), 순보험료_6월납 = ifelse(납입기간 == 
        0, 0, MxMx/NNxNNx_6월납/2), 순보험료_3월납 = ifelse(납입기간 == 
        0, 0, MxMx/NNxNNx_3월납/4), 순보험료_2월납 = ifelse(납입기간 == 
        0, 0, MxMx/NNxNNx_2월납/6), 
        순보험료_월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_월납/12), cePrime = tryCatch(cePrime, 
        error = function(z) return(0)), betaPrime = tryCatch(betaPrime, 
        error = function(z) return(0)), ce = tryCatch(ce, error = function(z) return(0)), 
        alpha2 = tryCatch(alpha2, error = function(z) return(0)), 
        alpha1 = tryCatch(alpha1, error = function(z) return(0)), 
        alphaPrime = tryCatch(alphaPrime, error = function(z) return(0)), 
        beta1 = tryCatch(beta1, error = function(z) return(0)), 
        beta2 = tryCatch(beta2, error = function(z) return(0)),
        베타순보험료_연납 = ifelse(납입기간 == 0, 순보험료_연납, 순보험료_연납 + (cePrime + alphaPrime + betaPrime) * 
                NNxNNx_납후/NNxNNx_연납), 베타순보험료_6월납 = ifelse(납입기간 == 
            0, 0, 순보험료_6월납 + (cePrime + alphaPrime + betaPrime) * 
            NNxNNx_납후/NNxNNx_6월납), 베타순보험료_3월납 = ifelse(납입기간 == 
            0, 0, 순보험료_3월납 + (cePrime + alphaPrime + betaPrime) * 
            NNxNNx_납후/NNxNNx_3월납), 베타순보험료_2월납 = ifelse(납입기간 == 
            0, 0, 순보험료_2월납 + (cePrime + alphaPrime + betaPrime) * 
            NNxNNx_납후/NNxNNx_2월납), 베타순보험료_월납 = ifelse(납입기간 == 
            0, 0, 순보험료_월납 + (cePrime + alphaPrime + betaPrime) * 
            NNxNNx_납후/NNxNNx_월납),
        영업보험료_연납 = ifelse(납입기간 == 0, (순보험료_연납)/(1 - alpha2 - beta1 - ce), (MxMx/NNxNNx_연납/1 + 보험가입금액*alpha2*Dx/NNxNNx_연납/1 + alpha1*기준연납순보험료*Dx/NNxNNx_연납/1)/(1 - beta1 - ce - beta2 - (betaPrime + cePrime)* NNxNNx_납후/NNxNNx_연납)), 
        영업보험료_6월납 = (MxMx/NNxNNx_월납/12 + 보험가입금액*alpha2*Dx/NNxNNx_6월납/2 + alpha1*기준연납순보험료*Dx/NNxNNx_6월납/2)/(1 - beta1 - ce - beta2 - (betaPrime + cePrime)* NNxNNx_납후/NNxNNx_연납), 
        영업보험료_3월납 = (MxMx/NNxNNx_3월납/4 + 보험가입금액*alpha2*Dx/NNxNNx_3월납/4 + alpha1*기준연납순보험료*Dx/NNxNNx_3월납/4)/(1 - beta1 - ce - beta2 - (betaPrime + cePrime)* NNxNNx_납후/NNxNNx_연납), 
        영업보험료_2월납 = (MxMx/NNxNNx_2월납/6 + 보험가입금액*alpha2*Dx/NNxNNx_2월납/6 + alpha1*기준연납순보험료*Dx/NNxNNx_2월납/6)/(1 - beta1 - ce - beta2 - (betaPrime + cePrime)* NNxNNx_납후/NNxNNx_연납), 
        영업보험료_월납 = (MxMx/NNxNNx_월납/12 + 보험가입금액*alpha2*Dx/NNxNNx_월납/12 + alpha1*기준연납순보험료*Dx/NNxNNx_월납/12)/(1 - beta1 - ce - beta2 - (betaPrime + cePrime)* NNxNNx_납후/NNxNNx_연납), 
        NxNx_r = (NxNx_r - (12 - 1)/(2 * 12) * DxDx_r), 
        납후보험료_연납 = 순보험료_연납 + 영업보험료_연납*(betaPrime + cePrime)*NNxNNx_납후/NNxNNx_연납,
        정기월납순보험료 = ifelse(납입기간 == 
            0, MxMx_r/Dxr, MxMx_r/NxNx_r/12), S값_개별 = 보험가입금액*(순보험료_월납/as.numeric(정기월납순보험료)), 
        표준해약공제액_단일률 = as.character(0.05 * 
            as.numeric(기준연납순보험료) * pmin(보험기간, 20) + 
            0.45 * as.numeric(기준연납순보험료), 2)) #%>% fwrite("PTtest_glas2.csv", bom = T)
      }







