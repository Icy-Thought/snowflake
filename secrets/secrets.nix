let
  thinkpad-e595 =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC9Y8pdy7XxPZu96YCXAtQ+ndlQ7NovoqwJZBUA6SYwf";
  sirius =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE0MHljC7KVnv+K2nyvGGY+Yu2Gst0lNx7jdRmiez5o1";

  probook-440g3 =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAm2MS+bLze5dvj6+Mde5mRAn4tfnh8K4PtFYSULE4fj";
  orca =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEhCXXL2cQz2nPQAb2q9olKirOyqDCEcElzl1Nr3QLlQ";

  users = [ sirius orca ];
  hosts = [ thinkpad-e595 probook-440g3 ];

in { "wg-akkad/privateKey.age".publicKeys = users ++ hosts; }
