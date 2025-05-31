{ pkgs, ... }:

{
  programs.emacs.init.usePackage.plantuml-mode = {
    enable = true;
    mode = [''"\\.plantuml\\'"'' ''"\\.puml\\'"''];
    custom = {
      org-plantuml-exec-mode = "'plantuml";
      # plantuml-default-exec-mode = "'executable";
      # plantuml-executable-path = ''"${pkgs.plantuml}/bin/plantuml"'';
      org-plantuml-executable-path = ''"${pkgs.plantuml}/bin/plantuml"'';
    }; 
    config = ''
      (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
      
      (defun hex-encode (str)
        (string-join (mapcar (lambda (c) (format "%02x" c)) str)))
      
      (defun plantuml-server-encode-url (string)
        "Encode the string STRING into a URL suitable for PlantUML server interactions."
        (let* ((encoded-string (hex-encode string)))
          (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))
    '';
  };
}
