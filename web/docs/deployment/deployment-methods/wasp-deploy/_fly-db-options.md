- `--db-vm-size <vmSize>` sets the database VM size. It defaults to `shared-cpu-1x`.
- `--db-vm-memory <vmMemory>` sets the database VM memory in MB.
- `--db-vm-cpus <vmCpus>` sets the database VM CPU count.
- `--db-vm-cpu-kind <vmCpuKind>` sets the database VM CPU kind.
- `--db-initial-cluster-size <initialClusterSize>` sets the initial number of database machines. It defaults to `1`.
- `--db-volume-size <volumeSize>` sets the database volume size in GB. It defaults to `1`.
- `--db-image <dbImage>` sets a custom PostgreSQL Docker image.

See Fly.io's [`postgres create` reference](https://fly.io/docs/flyctl/postgres-create/) for details and its [VM size documentation](https://fly.io/docs/flyctl/platform-vm-sizes/) for available machine sizes.
