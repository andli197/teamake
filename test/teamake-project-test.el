

(require 'teamake-project)
(require 'ert)



(ert-deftest teamake-project--get-current-configure-preset--can-return-last-configured-preset ()
  (should (string= (teamake-project--get-current-configure-preset
                    (list :current (list (cons 'teamake-preset
                                               (list (cons 'teamake-configure "Install-GCC-RelWithDebInfo"))))))
                   "Install-GCC-RelWithDebInfo")))

(ert-deftest teamake-project--get-current-configure-preset--return-empty-string-if-no-last-configure-preset ()
  (should (string= (teamake-project--get-current-configure-preset
                    (list :current '()))
                   "")))

(ert-deftest teamake-project--get-current-configure-preset--return-empty-string-if-no-current-exist-for-project ()
  (should (string= (teamake-project--get-current-configure-preset '()) "")))

(ert-deftest teamake-project-from-display--successfully-return-expected-project ()
  (let* ((teamake-project-configurations (list (list :name "Test project 1" :source-dir "/test/project/1")
                                               (list :name "Test project 2" :source-dir "/test/project/2")))
         (expected-project (car teamake-project-configurations)))
    (should (eq (teamake-project-from-display (teamake-project-display expected-project))
                expected-project))))

(ert-deftest teamake-project-from-display--successfully-return-expected-project ()
  (let* ((teamake-project-configurations (list (list :name "Test project 1" :source-dir "/test/project/1")
                                               (list :name "Test project 2" :source-dir "/test/project/2")))
         (expected-project (car teamake-project-configurations)))
    (should (eq (teamake-project-from-display (teamake-project-display expected-project))
                expected-project))))

(ert-deftest teamake-project-get-project-from-path--return-project-from-given-path ()
  (let* ((teamake-project-configurations (list (list :name "Test project 1" :source-dir "/test/project/1")
                                               (list :name "Test project 2" :source-dir "/test/project/2")))
         (expected-project (car teamake-project-configurations)))
    (should (eq (teamake-project-get-project-from-path "/test/project/1")
                expected-project))))

(ert-deftest teamake-directory-name--return-expected-directory-name-from-path ()
  (should (string= (teamake-directory-name "/test/project/1") "1"))
  (should (string= (teamake-directory-name "c:/test/project/2") "2")))

(ert-deftest teamake-project--get-current-values--return-current-values-for-prefix ()
  (let* ((expected-values (list "--file=correct"))
         (project (list :current (list (cons 'test-prefix-1 (list "--file=incorrect"))
                                       (cons 'test-prefix-2 expected-values)))))
    (should (eq (teamake-project--get-current-values project 'test-prefix-2)
                expected-values))))

(ert-deftest teamake-project--get-current-values--return-nil-when-no-prefix-or-current ()
  (should (eq (teamake-project--get-current-values (list :current '()) 'non-existing-prefix) '()))
  (should (eq (teamake-project--get-current-values (list :name "Project without any current") 'any-prefix) '())))

(ert-deftest teamake-project--get-current-configured-generator--return-generator-from-configuration ()
  (let ((project (list :current (list (cons 'teamake-configure (list "-G=Ninja"))))))
    (should (string= (teamake-project--get-current-configured-generator project)
                     "Ninja"))))
