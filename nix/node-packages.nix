# This file has been generated by node2nix 1.7.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {
    "argparse-1.0.10" = {
      name = "argparse";
      packageName = "argparse";
      version = "1.0.10";
      src = fetchurl {
        url = "https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz";
        sha512 = "o5Roy6tNG4SL/FOkCAN6RzjiakZS25RLYFrcMttJqbdd8BWrnA+fGz57iN5Pb06pvBGvl5gQ0B48dJlslXvoTg==";
      };
    };
    "asynckit-0.4.0" = {
      name = "asynckit";
      packageName = "asynckit";
      version = "0.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz";
        sha1 = "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79";
      };
    };
    "combined-stream-1.0.8" = {
      name = "combined-stream";
      packageName = "combined-stream";
      version = "1.0.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.8.tgz";
        sha512 = "FQN4MRfuJeHf7cBbBMJFXhKSDq+2kAArBlmRBvcvFE5BB1HZKXtSFASDhdlz9zOYwxh8lDdnvmMOe/+5cdoEdg==";
      };
    };
    "commander-2.20.3" = {
      name = "commander";
      packageName = "commander";
      version = "2.20.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/commander/-/commander-2.20.3.tgz";
        sha512 = "GpVkmM8vF2vQUkj2LvZmD35JxeJOLCwJ9cUkugyk2nuhbv3+mJvpLYYt+0+USMxE+oj+ey/lJEnhZw75x/OMcQ==";
      };
    };
    "component-emitter-1.3.0" = {
      name = "component-emitter";
      packageName = "component-emitter";
      version = "1.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/component-emitter/-/component-emitter-1.3.0.tgz";
        sha512 = "Rd3se6QB+sO1TwqZjscQrurpEPIfO0/yYnSin6Q/rD3mOutHvUrCAhJub3r90uNb+SESBuE0QYoB90YdfatsRg==";
      };
    };
    "cookiejar-2.1.2" = {
      name = "cookiejar";
      packageName = "cookiejar";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/cookiejar/-/cookiejar-2.1.2.tgz";
        sha512 = "Mw+adcfzPxcPeI+0WlvRrr/3lGVO0bD75SxX6811cxSh1Wbxx7xZBGK1eVtDf6si8rg2lhnUjsVLMFMfbRIuwA==";
      };
    };
    "core-util-is-1.0.2" = {
      name = "core-util-is";
      packageName = "core-util-is";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz";
        sha1 = "b5fd54220aa2bc5ab57aab7140c940754503c1a7";
      };
    };
    "debug-3.2.6" = {
      name = "debug";
      packageName = "debug";
      version = "3.2.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/debug/-/debug-3.2.6.tgz";
        sha512 = "mel+jf7nrtEl5Pn1Qx46zARXKDpBbvzezse7p7LqINmdoIk8PYP5SySaxEmYv6TZ0JyEKA1hsCId6DIhgITtWQ==";
      };
    };
    "delayed-stream-1.0.0" = {
      name = "delayed-stream";
      packageName = "delayed-stream";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz";
        sha1 = "df3ae199acadfb7d440aaae0b29e2272b24ec619";
      };
    };
    "esprima-4.0.1" = {
      name = "esprima";
      packageName = "esprima";
      version = "4.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/esprima/-/esprima-4.0.1.tgz";
        sha512 = "eGuFFw7Upda+g4p+QHvnW0RyTX/SVeJBDM/gCtMARO0cLuT2HcEKnTPvhjV6aGeqrCB/sbNop0Kszm0jsaWU4A==";
      };
    };
    "extend-3.0.2" = {
      name = "extend";
      packageName = "extend";
      version = "3.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/extend/-/extend-3.0.2.tgz";
        sha512 = "fjquC59cD7CyW6urNXK0FBufkZcoiGG80wTuPujX590cB5Ttln20E2UB4S/WARVqhXffZl2LNgS+gQdPIIim/g==";
      };
    };
    "form-data-2.5.1" = {
      name = "form-data";
      packageName = "form-data";
      version = "2.5.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/form-data/-/form-data-2.5.1.tgz";
        sha512 = "m21N3WOmEEURgk6B9GLOE4RuWOFf28Lhh9qGYeNlGq4VDXUlJy2th2slBNU8Gp8EzloYZOibZJ7t5ecIrFSjVA==";
      };
    };
    "formidable-1.2.2" = {
      name = "formidable";
      packageName = "formidable";
      version = "1.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/formidable/-/formidable-1.2.2.tgz";
        sha512 = "V8gLm+41I/8kguQ4/o1D3RIHRmhYFG4pnNyonvua+40rqcEmT4+V71yaZ3B457xbbgCsCfjSPi65u/W6vK1U5Q==";
      };
    };
    "graphlib-2.1.8" = {
      name = "graphlib";
      packageName = "graphlib";
      version = "2.1.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/graphlib/-/graphlib-2.1.8.tgz";
        sha512 = "jcLLfkpoVGmH7/InMC/1hIvOPSUh38oJtGhvrOFGzioE1DZ+0YW16RgmOJhHiuWTvGiJQ9Z1Ik43JvkRPRvE+A==";
      };
    };
    "inherits-2.0.4" = {
      name = "inherits";
      packageName = "inherits";
      version = "2.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz";
        sha512 = "k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==";
      };
    };
    "isarray-1.0.0" = {
      name = "isarray";
      packageName = "isarray";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz";
        sha1 = "bb935d48582cba168c06834957a54a3e07124f11";
      };
    };
    "js-yaml-3.13.1" = {
      name = "js-yaml";
      packageName = "js-yaml";
      version = "3.13.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/js-yaml/-/js-yaml-3.13.1.tgz";
        sha512 = "YfbcO7jXDdyj0DGxYVSlSeQNHbD7XPWvrVWeVUujrQEoZzWJIRrCPoyk6kL6IAjAG2IolMK4T0hNUe0HOUs5Jw==";
      };
    };
    "json-refs-2.1.7" = {
      name = "json-refs";
      packageName = "json-refs";
      version = "2.1.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/json-refs/-/json-refs-2.1.7.tgz";
        sha1 = "b9eb01fe29f5ea3e92878f15aea10ad38b5acf89";
      };
    };
    "lodash-4.17.15" = {
      name = "lodash";
      packageName = "lodash";
      version = "4.17.15";
      src = fetchurl {
        url = "https://registry.npmjs.org/lodash/-/lodash-4.17.15.tgz";
        sha512 = "8xOcRHvCjnocdS5cpwXQXVzmmh5e5+saE2QGoeQmbKmRS6J3VQppPOIt0MnmE+4xlZoumy0GPG0D0MVIQbNA1A==";
      };
    };
    "methods-1.1.2" = {
      name = "methods";
      packageName = "methods";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/methods/-/methods-1.1.2.tgz";
        sha1 = "5529a4d67654134edcc5266656835b0f851afcee";
      };
    };
    "mime-1.6.0" = {
      name = "mime";
      packageName = "mime";
      version = "1.6.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/mime/-/mime-1.6.0.tgz";
        sha512 = "x0Vn8spI+wuJ1O6S7gnbaQg8Pxh4NNHb7KSINmEWKiPE4RKOplvijn+NkmYmmRgP68mc70j2EbeTFRsrswaQeg==";
      };
    };
    "mime-db-1.43.0" = {
      name = "mime-db";
      packageName = "mime-db";
      version = "1.43.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/mime-db/-/mime-db-1.43.0.tgz";
        sha512 = "+5dsGEEovYbT8UY9yD7eE4XTc4UwJ1jBYlgaQQF38ENsKR3wj/8q8RFZrF9WIZpB2V1ArTVFUva8sAul1NzRzQ==";
      };
    };
    "mime-types-2.1.26" = {
      name = "mime-types";
      packageName = "mime-types";
      version = "2.1.26";
      src = fetchurl {
        url = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.26.tgz";
        sha512 = "01paPWYgLrkqAyrlDorC1uDwl2p3qZT7yl806vW7DvDoxwXi46jsjFbg+WdwotBIk6/MbEhO/dh5aZ5sNj/dWQ==";
      };
    };
    "ms-2.1.2" = {
      name = "ms";
      packageName = "ms";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/ms/-/ms-2.1.2.tgz";
        sha512 = "sGkPx+VjMtmA6MX27oA4FBFELFCZZ4S4XqeGOXCv68tT+jb3vk/RyaKWP0PTKyWtmLSM0b+adUTEvbs1PEaH2w==";
      };
    };
    "native-promise-only-0.8.1" = {
      name = "native-promise-only";
      packageName = "native-promise-only";
      version = "0.8.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/native-promise-only/-/native-promise-only-0.8.1.tgz";
        sha1 = "20a318c30cb45f71fe7adfbf7b21c99c1472ef11";
      };
    };
    "path-loader-1.0.10" = {
      name = "path-loader";
      packageName = "path-loader";
      version = "1.0.10";
      src = fetchurl {
        url = "https://registry.npmjs.org/path-loader/-/path-loader-1.0.10.tgz";
        sha512 = "CMP0v6S6z8PHeJ6NFVyVJm6WyJjIwFvyz2b0n2/4bKdS/0uZa/9sKUlYZzubrn3zuDRU0zIuEDX9DZYQ2ZI8TA==";
      };
    };
    "process-nextick-args-2.0.1" = {
      name = "process-nextick-args";
      packageName = "process-nextick-args";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.1.tgz";
        sha512 = "3ouUOpQhtgrbOa17J7+uxOTpITYWaGP7/AhoR3+A+/1e9skrzelGi/dXzEYyvbxubEF6Wn2ypscTKiKJFFn1ag==";
      };
    };
    "punycode-2.1.1" = {
      name = "punycode";
      packageName = "punycode";
      version = "2.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz";
        sha512 = "XRsRjdf+j5ml+y/6GKHPZbrF/8p2Yga0JPtdqTIY2Xe5ohJPD9saDJJLPvp9+NSBprVvevdXZybnj2cv8OEd0A==";
      };
    };
    "qs-6.9.1" = {
      name = "qs";
      packageName = "qs";
      version = "6.9.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/qs/-/qs-6.9.1.tgz";
        sha512 = "Cxm7/SS/y/Z3MHWSxXb8lIFqgqBowP5JMlTUFyJN88y0SGQhVmZnqFK/PeuMX9LzUyWsqqhNxIyg0jlzq946yA==";
      };
    };
    "readable-stream-2.3.7" = {
      name = "readable-stream";
      packageName = "readable-stream";
      version = "2.3.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.7.tgz";
        sha512 = "Ebho8K4jIbHAxnuxi7o42OrZgF/ZTNcsZj6nRKyUmkhLFq8CHItp/fy6hQZuZmP/n3yZ9VBUbp4zz/mX8hmYPw==";
      };
    };
    "safe-buffer-5.1.2" = {
      name = "safe-buffer";
      packageName = "safe-buffer";
      version = "5.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz";
        sha512 = "Gd2UZBJDkXlY7GbJxfsE8/nvKkUEU1G38c1siN6QP6a9PT9MmHB8GnpscSmMJSoF8LOIrt8ud/wPtojys4G6+g==";
      };
    };
    "slash-1.0.0" = {
      name = "slash";
      packageName = "slash";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/slash/-/slash-1.0.0.tgz";
        sha1 = "c41f2f6c39fc16d1cd17ad4b5d896114ae470d55";
      };
    };
    "sprintf-js-1.0.3" = {
      name = "sprintf-js";
      packageName = "sprintf-js";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz";
        sha1 = "04e6926f662895354f3dd015203633b857297e2c";
      };
    };
    "string_decoder-1.1.1" = {
      name = "string_decoder";
      packageName = "string_decoder";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/string_decoder/-/string_decoder-1.1.1.tgz";
        sha512 = "n/ShnvDi6FHbbVfviro+WojiFzv+s8MPMHBczVePfUpDJLwoLT0ht1l4YwBCbi8pJAveEEdnkHyPyTP/mzRfwg==";
      };
    };
    "superagent-3.8.3" = {
      name = "superagent";
      packageName = "superagent";
      version = "3.8.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/superagent/-/superagent-3.8.3.tgz";
        sha512 = "GLQtLMCoEIK4eDv6OGtkOoSMt3D+oq0y3dsxMuYuDvaNUvuT8eFBuLmfR0iYYzHC1e8hpzC6ZsxbuP6DIalMFA==";
      };
    };
    "uri-js-3.0.2" = {
      name = "uri-js";
      packageName = "uri-js";
      version = "3.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/uri-js/-/uri-js-3.0.2.tgz";
        sha1 = "f90b858507f81dea4dcfbb3c4c3dbfa2b557faaa";
      };
    };
    "util-deprecate-1.0.2" = {
      name = "util-deprecate";
      packageName = "util-deprecate";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
        sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
      };
    };
    "yaml-js-0.1.5" = {
      name = "yaml-js";
      packageName = "yaml-js";
      version = "0.1.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/yaml-js/-/yaml-js-0.1.5.tgz";
        sha1 = "a01369010b3558d8aaed2394615dfd0780fd8fac";
      };
    };
  };
  args = {
    name = "multi-file-swagger";
    packageName = "multi-file-swagger";
    version = "2.0.0";
    src = ../../../../../nix/store/b4vpkhq273fk0jwqz0hwyc3493wd9cpf-source/multi-file-swagger;
    dependencies = [
      sources."argparse-1.0.10"
      sources."asynckit-0.4.0"
      sources."combined-stream-1.0.8"
      sources."commander-2.20.3"
      sources."component-emitter-1.3.0"
      sources."cookiejar-2.1.2"
      sources."core-util-is-1.0.2"
      sources."debug-3.2.6"
      sources."delayed-stream-1.0.0"
      sources."esprima-4.0.1"
      sources."extend-3.0.2"
      sources."form-data-2.5.1"
      sources."formidable-1.2.2"
      sources."graphlib-2.1.8"
      sources."inherits-2.0.4"
      sources."isarray-1.0.0"
      sources."js-yaml-3.13.1"
      sources."json-refs-2.1.7"
      sources."lodash-4.17.15"
      sources."methods-1.1.2"
      sources."mime-1.6.0"
      sources."mime-db-1.43.0"
      sources."mime-types-2.1.26"
      sources."ms-2.1.2"
      sources."native-promise-only-0.8.1"
      sources."path-loader-1.0.10"
      sources."process-nextick-args-2.0.1"
      sources."punycode-2.1.1"
      sources."qs-6.9.1"
      sources."readable-stream-2.3.7"
      sources."safe-buffer-5.1.2"
      sources."slash-1.0.0"
      sources."sprintf-js-1.0.3"
      sources."string_decoder-1.1.1"
      sources."superagent-3.8.3"
      sources."uri-js-3.0.2"
      sources."util-deprecate-1.0.2"
      sources."yaml-js-0.1.5"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Multi-file Swagger example";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
in
{
  tarball = nodeEnv.buildNodeSourceDist args;
  package = nodeEnv.buildNodePackage args;
  shell = nodeEnv.buildNodeShell args;
}