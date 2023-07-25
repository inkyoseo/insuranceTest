#' Title Convert/calculate Gisu-table to Reserve table
#'
#' @param 기수표 Gisu
#' @param exp
#'
#' @return Reserve table #
#' @export
#'
#' @examples
gisu2reserve  <- function(기수표, exp) {기수표 %>%
    #        filter(seq %in% c(1,2, 3)) %>%
    #        col_gisu2reserve <- 기수표 %>% head(1) %>% select(-contains(c("D", "N", "C", "M"))) %>% colnames
    group_by(seq) %>%
    mutate(
      # 기준 연납순보험료 구송요소
      납입기간 = min(납입기간),
      MxMx_s = list(Mx[t==0] - Mx[t==보험기간]),
      NNxNNx_s =list(NNx[t==0] - NNx[t==pmin(납입기간,20)]),

      # 순보험료 구성요소
      NxNx_납후 = list(Nx[t==납입기간] - Nx[t==보험기간]),
      NxNx = list(Nx[t==납입기간] - Nx[t==납입기간]),
      MxMx =  list(Mx[t==0] - Mx[t==보험기간]),
      DxDx = list(Dx[t==0] - Dx[t==보험기간]),
      DDxDDx = list(DDx[t==0] - DDx[t==납입기간]),
      NNxNNx = list(NNx[t==0] - NNx[t==납입기간])
    ) %>%
    merge(exp) %>%

    rowwise() %>%
    mutate(
      # 선형근사
      NNxNNx_연납 = (NNxNNx - (1-1)/(2*1)*DDxDDx),
      NNxNNx_6월납 = (NNxNNx - (2-1)/(2*2)*DDxDDx),
      NNxNNx_3월납 = (NNxNNx - (4-1)/(2*4)*DDxDDx),
      NNxNNx_2월납 = (NNxNNx - (6-1)/(2*6)*DDxDDx),
      NNxNNx_월납 = (NNxNNx - (12-1)/(2*12)*DDxDDx),

      # 보험료
      ## 기준연납순보험료
      기준연납순보험료 = ifelse(납입기간 == 0, 0, MxMx_s/NNxNNx_s),

      ## 순보험료
      순보험료_연납 = ifelse(납입기간 == 0, MxMx/Dx, MxMx/NNxNNx_연납/1),
      순보험료_6월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_6월납/2),
      순보험료_3월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_3월납/4),
      순보험료_2월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_2월납/6),
      순보험료_월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_월납/12),

    ) %>%
    group_by(seq) %>%
    mutate(MtMx = Mx - last(Mx),
           NNtNNx = ifelse(납입기간<t, 0, NNx - nth(NNx, max(납입기간+1)))) %>%
    rowwise() %>%
    #mutate(Vx = ifelse(납입기간<t, MtMx/Dx, (MtMx - 순보험료_연납*(NNtNNx))/Dx))
    mutate(Vt = (MtMx - 순보험료_연납*(NNtNNx))/Dx)

  #            mutate(across(where(is.numeric), round, 8))    # 사용x
  #            mutate_if(is.numeric, round, 8)                # 사용x

}
#        select(seq, contains("보험료")) %>%
#        select(-contains("베타")) %>%
#        mutate_if(is.numeric, round)
