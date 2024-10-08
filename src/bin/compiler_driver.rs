use anyhow::Result;
use clap::Parser;
use compiler_c_sandler::{CliArgs, Driver};

fn main() -> Result<()> {
    env_logger::init();

    let args = CliArgs::parse();
    log::info!("{args:?}");

    let driver = Driver::from(args);
    driver.run()
}
