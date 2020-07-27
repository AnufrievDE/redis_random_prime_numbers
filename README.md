# Random Prime Numbers (Redis Test App)

This is erlang application created to show up a way of working with Redis KV storage.

There are 2 gen_server processes:
  * rpn_generator - uniformly generates random numbers from 2 to given `N` with given `Numbers per second` requirement, and push them to given `Redis List Key`;
  * rpn_filter - once a second fetches no more than 2*`N` numbers from `Redis List Key`, filter them for prime numbers and pushes result to the given `Redis Result Set Key`.

## Configuration parameters (with default values):
  * `manual_start`(`false`) - postpones timers initialization if true;
  * `numbers_per_sec`(`3000`) - number of random numbers to generate and push to redis per second;
  * `n` - random number upper boarder;
  * `rdb_list_key` - redis list key to push random numbers in;
  * `rdb_result_set_key` - redis result set key to push prime numbers in;
  * `pools` - several options for managing pool of connections to redis.

## Docker-compose run
  1. check .env, sys.config.src file to set needed configuration parameters
  2. run:
     * `docker-compose up --build --force-recreate`
  3. or build and run:
     * `docker-compose build`
     * `docker-compose up`

## Local run
1. check sys.config file to set needed configuration parameters
2. make sure you have redis running on configured host/port
3. run  
   * `rebar3 as local shell` 
4. or build and run
   * `rebar3 as local release`
   * `_build/local/rel/rpn/bin/rpn console|foreground`

## Run Tests
`rebar3 eunit`