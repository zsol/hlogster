module Input (inputlines) where

-- inp is a simulation of a lof file stream
s = "2012-06-01 01:23:03,657 app6 usage INFO 264:presentations_delete User 10179708 deleted presentation 30334553 (owner: 10179708, title: Frank Macfarlane Burnet, public: 0, version: 14, created: 2012-05-"
s2 = "2012-06-02 18:34:43,952 domU-12-31-38-00-C1-72 storage INFO 13:log_timing timing read_preview 10.852 0701.static.prezi.com 30588598"
inputlines = (take 5 (repeat s)) ++ (take 5 (repeat s2))