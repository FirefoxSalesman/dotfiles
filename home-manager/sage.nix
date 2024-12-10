{ pkgs, ... }:
{
  home.packages = with pkgs; [ sage ];

  programs.emacs.init.usePackage = {
    sage-shell-mode = {
      enable = true;
      defer = true;
      custom."sage-shell-edit:display-function" = ''"display-buffer"'';
      config = ''(sage-shell:define-alias)'';
      generalOne."efs/leader-keys"."S" = '''(run-sage :which-key "sage")'';
      generalTwo."local-leader"."sage-shell-mode-map"."h" = "'sage-shell:help";
    };
  
    ob-sagemath = {
      enable = true;
      afterCall = ["sage-shell-mode" "ob"];
    };
  };

  home.file.".config/sage/ipython-5.0.0/profile_default/startup/00nix.py".text = ''
    # Combo functions
    def permutation(n, r):
      return factorial(n)/factorial(n - r)
    
    def combo(n, r):
      return permutation(n, r)/factorial(r)
    
    # Probstats functions
    def pbinary(n, p, x):
      """Returns the probability of a binary distribution"""
      return combo(n, x)*(p**x)*((1-p)**(n-x))
    
    def ebinary(n, p):
      """Returns the expectation (mean) of a binary distribution"""
      return n*p
    
    def ppoisson(x, lam):
      """Returns the probability of the poisson distribution"""
      return (exp(-lam)*lam**x)/factorial(x)
    def poisprocess(lam, s, t, n):
      """Returns the probability of the poisson process
            lam: lambda (the rate)
            s: the start time
            t: the end time
            n: the number of occurrances"""
      return (((lam*(t - s))**n)*exp(-lam*(t - s)))/factorial(n)
    
    def expecpoisprocess(lam, s, t):
      """Returns the expectancy of a poisson process
           lam: lambda (the rate)
            s: the start time
            t: the end time"""
      return lam*(t-s)
    
    def pnormal(x, mu, sigma):
      """Returns the probability of a normal random variable
         x: the other variable
         mu: the expectancy
         sigma: the variance"""
      return exp(-((x-mu)**2)/(2*sigma))/sqrt(2*pi*sigma)
  '';
}
