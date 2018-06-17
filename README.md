# BacktestR
Backtest written in R

TODOs:
Grid Search project:
1) create metrics: TRR, EXR, sharpe, sortino, duration, ASW for index, port no tx, port
    a) function to calculate each of the metrics using dataset for each index
    b) write results to a file
2) Calculate above metrics over multiple periods: full period, other pre-defined periods (bulls and bears)
    a) config file to store different periods
    b) UI to update/save config file
3) Setup to adjust current score to opinion calculation
4) Setup to include additional scores such as : ESG or liquidity 
5) Setup for portfolio building, such as different weights for outperform, neutral and underperform 