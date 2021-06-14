
var y       ${y}$                   (long_name='output')
    pi      ${\pi}$                 (long_name='inflation')
    i       ${i}$                   (long_name='nominal interrst rate')
    a       ${a}$                   (long_name='AR(1) technology shock process')
    rn      ${r^{N}}$             (long_name='natural interest rate')
    n       ${n}$                   (long_name='hours worked')
    v       ${\nu}$                 (long_name='AR(1) monetary policy shock process')   
    y_gap   ${\hat{y}}$            (long_name='output gap')
    y_nat   ${y^{N}}$             (long_name='natural output');  

varexo e_a  ${\varepsilon_a}$       (long_name='technology shock')
       e_v  ${\varepsilon_\nu}$     (long_name='monetary policy shock'); 

parameters alpha     ${\alpha}$   
           beta      ${\beta}$   
           theta     ${\theta}$   
           sigma     ${\sigma}$   
           phi       ${\phi}$   
           rho       ${\rho}$   
           phi_pi    ${\phi_pi}$   
           phi_y     ${\phi_y}$   
           rho_a     ${\rho_a}$   
           rho_v     ${\rho_v}$   
           lambda    ${\lambda}$   
           kappa     ${\kappa}$   
           psi       ${\psi}$   
           epsilon   ${\epsilon}$;   

// These values follow the (corrected) estimation of Galí, Gertler & López-Salido (2001/3) for the US, specification (2) in Table 1.
alpha=.27;              // If =0 we have a CRS production technology. Else it's decreasing returns to scale (see model equation 5).
epsilon=1.5;            // Elasticity of substitution derived from the markup forumla m=log(epsilon/(epsilon-1)). Using m=1.1.
beta=0.923;             // The discount factor. 
theta=0.698;            // Measure of price stickiness. If =0 then prices are flexible.
rho=-log(beta);         // Real interest rate in the steady state (no shocks).

sigma=1;                // Coefficient of risk aversion.
phi=1;                  // Elasticity of labor supply.
phi_pi=1.5;             // Sensitivity of the central bank with respect to inflation.
phi_y=0.5/4;             // Sensitivity of the central bank with respect to the output gap.
rho_a=0.975;            // Persistence of the technology shock.
rho_v=0.5;              // Persistence of the monetary policy shock.


// The next two parameters are generated for the solution of the model. Note that when alpha=0, these equations get much easier.
lambda=(theta^(-1))*(1-theta)*(1-beta*theta)*(1-alpha)/(1-alpha+alpha*epsilon);
kappa=lambda*(sigma+(phi+alpha)/(1-alpha));
psi=(1+phi)*((sigma+phi+alpha*(1-sigma))^(-1));


model;                                      
y_gap=y_gap(+1)-1/sigma*(i-pi(+1)-rn);      // Eq. 1: The Dynamic IS equation.
pi=beta*pi(+1)+kappa*y_gap;                 // Eq. 2: The New Keynesian Philips Curve.
rn=rho+sigma*psi*(a(+1)-a);                 // Eq. 3: The evolution of the natural rate of interest.
i=rho+phi_pi*pi+phi_y*y_gap+v;              // Eq. 4: The interest rate rule of the central bank.
y_gap = y-y_nat;                            // Eq. 5: The definition of output gap 
y=a+(1-alpha)*n;                            // Eq. 6: The production function consisting of technology and labor. This relationship is only true up to a 1st order approximation.
a=rho_a*a(-1)+e_a;                          // Eq. 7: Technology shocks follow an AR(1) process with persistence rho_a.
v=rho_v*v(-1)+e_v;                          // Eq. 8: Monetary policy follow an AR(1) process with persistence rho_v.
y_nat = psi*a;                              // Eq. 9: The definition of natural output

end;

initval;
y=0;
n=0;
pi=0;
i=rho;
rn=rho;
a=0;
v=0;
e_a=0;
e_v=0;
end;

steady;
check;

shocks;
var e_a;
stderr 1;
var e_v;
stderr 1;        
end;

// The above equations only hold up to a first order approximation. Thus order=1 for the simulation.
stoch_simul(irf=12, nofunctions, order=1) n y i rn pi a v; 
write_latex_static_model;
