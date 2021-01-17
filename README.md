# Laminar-Boundary-Layer
The problem here addressed is the one of solving the Navier-Stokes equations in derived variables, particularly for the streamfunction and vorticity variables. The aim is to be able to study a flow over a flat plate in a boundary layer scenario, that is, with an irrotational far-field away from the plate where one of the velocity components is given. We avoid the use of the traditional primitive variables, velocity and pressure, because the boundary conditions in the far-field are ill-conditioned. In that formulation, one cannot assure continuity and irrotationality at the same time, obliging the user to have huge computational domains to avoid results contamination. 

We have discretized the non-linear system of equations that arise in this derived formulation via a Finite Differences method. This non-linear algebraic system is then solved using a Newton-Rapshon procedure. This step is of critical importance, since both equations need to be solved simultaneously to guarantee numerical and physical robustness. This is the case because no proper boundary condition can be derived through physical reasoning for the vorticity value or flux at the boundary with a rigid wall. 
To solve the constantly changing Jacobians of the linearized system that are used in this procedure, we have employed a *matrix-free* iterative linear solver, the Generalised Minimal Residual (GMRES). 

The main issue faced by the project was how to deal with the boundary condition for the vorticity at the wall. We coded many other approaches beforehand: solving the biharmonic equation, using a transitory approach with artificial boundary conditions and even a boundary element method in order to enforce an integral constraint. At the end, the simultaneous solution of both equations proved to be the way.  

Once the code was validated by means of the Method of Manufactured Solutions, some preliminary results on different pressure gradients over the flow could be obtained. We present a recirculation bubble, a favourable pressure gradient (FPG) and a zero pressure gradient (ZPG) boundary layers. We start to see here how some classical results from the boundary layer solutions are valid and others not so much. The physical domain corresponds to almost a hundred initial boundary layer thickness on the stream-wise direction, and about ten to twenty on the transversal one. Although useful remarks can be drawn from these results, larger domains still need to be studied. 

Coding a DNS code from almost scratch is not at all trivial, specially if flexibility in the possible scenarios to study is a condition on its design. It does not only suffice to compute the derivatives and solve the linear system, it is challenging to understand the behaviour of the code, or if the real physics are been tracked and no spurious solutions are being introduced. In a sense, those who do DNS are very close to craftsmanship, where sometimes specific solutions are required, despite the general and abstract mathematical context of the all. 

--- 

It is not actually a working code base, since some of it components are to remain private.

This was done as part of my intership at the Laboratory for Turbulence Research in Aerospace and Combustion (LTRAC) in the Department of Mechanical and Aerospace Engineering Monash University, in Melbourne, Australia.
