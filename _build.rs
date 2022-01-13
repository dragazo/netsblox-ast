use std::fs::File;
use std::io::{Write, BufWriter};
use std::collections::BTreeMap;

use hyper::{body, Body, Client, client::HttpConnector};
use hyper_tls::HttpsConnector;
use serde_json::Value as Json;
use percent_encoding::{utf8_percent_encode as url_encode, CONTROLS, AsciiSet};

const SERVICES_URL: &str = "https://editor.netsblox.org/services";

type Cli = Client<HttpsConnector<HttpConnector>>;

async fn get(client: &Cli, url: &str) -> Json {
    let resp = client.get(url.parse().unwrap()).await.unwrap();
    assert!(resp.status().is_success());
    let bytes = body::to_bytes(resp.into_body()).await.unwrap();
    serde_json::from_slice(&bytes).unwrap()
}

#[tokio::main]
async fn main() {
    const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

    let https = HttpsConnector::new();
    let client = Client::builder().build::<_, Body>(https);

    let services = get(&client, SERVICES_URL).await;
    let services: Vec<_> = services.as_array().unwrap().into_iter().map(|v| {
        v["name"].as_str().unwrap()
    }).collect();

    let mut services_info = BTreeMap::default();
    for &service in services.iter() {
        println!("{}", service);
        let meta = get(&client, &format!("{}/{}", SERVICES_URL, url_encode(service, FRAGMENT))).await;
        let rpcs = meta["rpcs"].as_object().unwrap();

        let mut rpcs_info = BTreeMap::default();
        for (rpc, info) in rpcs {
            let args: Vec<_> = info["args"].as_array().unwrap().into_iter().map(|v| {
                v["name"].as_str().unwrap().to_owned()
            }).collect();
            if let Some(_) = rpcs_info.insert(rpc.to_owned(), args) {
                panic!("duplicate rpc: {}", rpc);
            }
        }
        if let Some(_) = services_info.insert(service.to_owned(), rpcs_info) {
            panic!("duplicate service: {}", service);
        }
    }

    let mut f = BufWriter::new(File::create("src/rpcs.rs").unwrap());
    write!(&mut f, r#"use std::collections::BTreeMap;
lazy_static! {{
    static ref SERVICE_INFO: BTreeMap<&'static str, BTreeMap<&'static str, &'static [&'static str]>> = {{
        let mut services = BTreeMap::default();
"#).unwrap();
    for (service, rpcs) in services_info.iter() {
        write!(&mut f, "        services.insert(\"{}\", {{\n            let mut rpcs = BTreeMap::default();\n", service).unwrap();
        for (rpc, args) in rpcs.iter() {
            let trans_args: Vec<_> = args.iter().map(|v| format!("\"{}\"", v.replace('"', "\\\""))).collect();
            write!(&mut f, "            rpcs.insert(\"{}\", &[{}]);\n", rpc, trans_args.join(", ")).unwrap();
        }
        write!(&mut f, "            rpcs\n        }});\n").unwrap();
    }
    write!(&mut f, "    services\n    }};\n}}\n").unwrap();
}
