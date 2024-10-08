use anyhow::{anyhow, Result};
use clap::Parser;
use compiler_c_sandler::{CliArgs, Driver};

fn main() -> Result<()> {
    env_logger::init();

    let args = CliArgs::parse();
    log::info!("{args:?}");

    let driver = Driver::from(args);
    let res = driver.run();

    res.map_err(|e| {
        /* Omit error details, if so configured.
        `env_logger` dependency enables the `error` level by default.
        Use case: when testing, reduce i/o. */
        if log::log_enabled!(log::Level::Error) {
            e
        } else {
            anyhow!("Failed! To see details, turn on log level `error` or lower.")
        }
    })
}
