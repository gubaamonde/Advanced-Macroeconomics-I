/*
 * FGV/EESP - Advanced Macroeconomics I
 *
 * This file implements the baseline New Keynesian model of Jordi Galí (2008): 
 * Monetary Policy, Inflation, and the Business Cycle. 
 * Princeton University Press, First Edition, Chapter 3.
 *
 * Important: the model equations and FOC's are obtained WITHOUT linearization.
 *
 * 06.06.2021 */

    
%----------------------------------------------------------------
% Define Variables
%----------------------------------------------------------------

var 

    C               ${C}$           (long_name='Consumption')
    W_real          ${\frac{W}{P}}$ (long_name='Real Wage')
    Pi              ${\Pi}$         (long_name='inflation')
    A               ${A}$           (long_name='AR(1) technology process')
    N               ${N}$           (long_name='Hours worked')
    R               ${R^n}$         (long_name='Nominal Interest Rate') 
    R_real          ${R^{r}}$       (long_name='Real Interest Rate')
    Y               ${Y}$           (long_name='Output') 
    Q               ${Q}$           (long_name='Bond price')
    D               ${D}$           (long_name='Price dispersion')
    Pi_star         ${\Pi^*}$       (long_name='Optimal reset price')
    x_aux_1         ${x_1}$         (long_name='aux. var. 1 recursive price setting')
    x_aux_2         ${x_2}$         (long_name='aux. var. 2 recursive price setting')
    MC              ${mc}$          (long_name='real marginal costs')
    i_ann           ${i^{ann}}$     (long_name='annualized nominal interest rate')
    pi_ann          ${\pi^{ann}}$   (long_name='annualized inflation rate')
    r_real_ann      ${r^{r,ann}}$   (long_name='annualized real interest rate')
    P               ${P}$           (long_name='price level')
    log_y           ${log(M)}$      (long_name='log output')
    log_W_real      ${log(W/P)}$    (long_name='log real wage')
    log_N           ${log(N)}$      (long_name='log hours')
    log_P           ${log(P)}$      (long_name='log price level')
    log_A           ${log(A)}$      (long_name='log technology level')
    nu              ${\nu}$         (long_name='AR(1) monetary policy shock process')

;    

%----------------------------------------------------------------
% Define Shock Variables
%----------------------------------------------------------------

varexo 

       eps_a           ${\varepsilon_a}$          (long_name='technology shock')
       eps_nu          ${\varepsilon_\nu}$        (long_name='monetary policy shock')

;       

%----------------------------------------------------------------
% Define Parameters
%----------------------------------------------------------------

parameters   
    alppha              ${\alpha}$          (long_name='capital share')
    betta               ${\beta}$           (long_name='discount factor')
    rho_a               ${\rho_a}$          (long_name='autocorrelation technology shock')
    rho_nu              ${\rho_{\nu}}$      (long_name='autocorrelation monetary policy shock')
    siggma              ${\sigma}$          (long_name='inverse EIS')
    varphi              ${\varphi}$         (long_name='inverse Frisch elasticity')
    phi_pi              ${\phi_{\pi}}$      (long_name='inflation feedback Taylor Rule')
    phi_y               ${\phi_{y}}$        (long_name='output feedback Taylor Rule')
    epsilon             ${\epsilon}$        (long_name='demand elasticity')
    theta               ${\theta}$          (long_name='Calvo parameter')

;    

%----------------------------------------------------------------
% Parametrization, p. 67  and p. 113-115
%----------------------------------------------------------------

% Parameters
    siggma      = 2;
    varphi      = 1;
    phi_y       = 0.16;
    theta       = 2/3;
    betta       = 0.985;
    alppha      = 1/3;
    epsilon     = 6;

% Policy Parameters
    phi_pi      = 1.5;

% AR shock parameters
    rho_nu      = 0.5;
    rho_a       = 0.90;

%----------------------------------------------------------------
% First Order Conditions
%----------------------------------------------------------------

model;

    [name='Labor demand']
    W_real=C^siggma*N^varphi;

    [name='Euler equation']
    Q=betta*(C(+1)/C)^(-siggma)/Pi(+1);

    [name='Definition nominal interest rate)']
    R=1/Q;

    [name='Aggregate output']
    Y=A*(N/D)^(1-alppha);

    [name='Definition Real interest rate']
    R=R_real*Pi(+1);

    [name='Monetary Policy Rule']
    R=1/betta*Pi^phi_pi*(Y/steady_state(Y))^phi_y*exp(nu);

    [name='Market Clearing']
    Y = C;

	[name='Technology shock']
    log(A)=rho_a*log(A(-1))+eps_a;

	[name='Monetary policy shock']
    nu=rho_nu*nu(-1)+eps_nu;

    [name='Definition marginal cost']
    MC=W_real/((1-alppha)*Y/N*D);

    [name='Aggregate prices']
    1=theta*Pi^(epsilon-1)+(1-theta)*(Pi_star)^(1-epsilon);

    [name='Price dispersion']
    D=(1-theta)*Pi_star^(-epsilon/(1-alppha))+theta*Pi^(epsilon/(1-alppha))*D(-1);

    [name='FOC price setting']
    Pi_star^(1+epsilon*(alppha/(1-alppha)))=x_aux_1/x_aux_2*epsilon/(epsilon-1);

    [name='Auxiliary price setting recursion 1']
    x_aux_1=C^(-siggma)*Y*MC+betta*theta*Pi(+1)^(epsilon+alppha*epsilon/(1-alppha))*x_aux_1(+1);

    [name='Auxiliary price setting recursion 2']
    x_aux_2=C^(-siggma)*Y+betta*theta*Pi(+1)^(epsilon-1)*x_aux_2(+1);

    [name='Definition price level']
    Pi=P/P(-1);

    [name='Definition log output']
    log_y = log(Y);

    [name='Definition log real wage']
    log_W_real=log(W_real);

    [name='Definition log hours']
    log_N=log(N);

    [name='Annualized inflation']
    pi_ann=4*log(Pi);

    [name='Annualized nominal interest rate']
    i_ann=4*log(R);

    [name='Annualized real interest rate']
    r_real_ann=4*log(R_real);

    [name='Definition log price level']
    log_P=log(P);

    [name='Definition log TFP']
    log_A=log(A);


end;

%---------------------------------------------------------------
%  Steady state values
%---------------------------------------------------------------

steady_state_model;

    A=1;
    Z=1;
    D=1;
    Pi_star=1;
    P=1;
    MC=(epsilon-1)/epsilon;
    R=1/betta;
    Pi=1;
    Q=1/R;
    R_real=R;
    N=((1-alppha)*MC)^(1/((1-siggma)*alppha+varphi+siggma));
    C=A*N^(1-alppha);
    W_real=C^siggma*N^varphi;
    Y=C;
    nu=0;
    x_aux_1=C^(-siggma)*Y*MC/(1-betta*theta*Pi^(epsilon/(1-alppha)));
    x_aux_2=C^(-siggma)*Y/(1-betta*theta*Pi^(epsilon-1));
    log_y = log(Y);
    log_W_real=log(W_real);
    log_N=log(N);
    pi_ann=4*log(Pi);
    i_ann=4*log(R);
    r_real_ann=4*log(R_real);
    log_P=log(P);
    log_A=0;
    log_Z=0;

end;

resid(1);
steady;
check;

%----------------------------------------------------------------
% Define Shocks' Variance
%----------------------------------------------------------------

shocks;

    var eps_nu      = 0.25^2;
    var eps_a       = 0.25^2;
   
end;

%----------------------------------------------------------------
% Solve the model and simulate a montery policy shock
%----------------------------------------------------------------

stoch_simul(order = 1, irf=30, irf_shocks = (eps_nu, eps_a)) log_y C pi_ann i_ann r_real_ann P;
write_latex_static_model;

