#!/bin/bash

# This script runs Playwright tests for specified Wasp example applications.

# Applications to be tested in both 'dev' and 'build' modes
apps_full_test=(
    "examples/waspello"
    "examples/waspleau"
    "examples/websockets-realtime-voting"
    "waspc/examples/todoApp"
)

# Applications to be tested in 'dev' mode only
apps_dev_only=(
    "examples/tutorials/TodoApp"
    "examples/tutorials/TodoAppTs"
)

# Base Playwright command
PLAYWRIGHT_CMD_BASE="npx playwright test --config headless-tests/"

# Terminal colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m' 
BOLD='\033[1m'
NC='\033[0m' # No Color

# Stats tracking
total_apps_tested=0
passed_apps_count=0
failed_apps_count=0
failed_apps=()

# Function to run Playwright tests for a specific mode
run_tests_for_mode() {
    local app_dir="$1"
    local mode="$2"
    
    echo -e "\n${BLUE}Testing (${mode} mode): ${app_dir}${NC}"
    
    # Temporarily disable errexit if set, to handle test failures locally
    set +e
    HEADLESS_WASP_CLI_CMD=wasp-cli HEADLESS_TEST_MODE=$mode DEBUG=pw:webserver $PLAYWRIGHT_CMD_BASE
    local test_status=$?
    set -e # Re-enable errexit if it was set
    
    if [ $test_status -ne 0 ]; then
        echo -e "${RED}✗ FAILED: ($mode mode) tests for $app_dir${NC}"
        return 1
    else
        echo -e "${GREEN}✓ PASSED: ($mode mode) tests for $app_dir${NC}"
        return 0
    fi
}

# Function to run tests for a single application
run_tests_for_app() {
    local app_dir="$1"
    shift # Remaining arguments are test modes
    
    echo -e "\n${YELLOW}${BOLD}Testing application: $app_dir${NC}"
    
    local app_start_time=$(date +%s)
    
    if ! pushd "$app_dir" > /dev/null; then
        echo -e "${RED}ERROR: Failed to change directory to $app_dir${NC}"
        return 1
    fi
    
    echo -e "${BLUE}Installing dependencies in $(pwd)...${NC}"
    if ! npm ci; then
        echo -e "${RED}ERROR: npm ci failed in $(pwd)${NC}"
        popd > /dev/null
        return 1
    fi

    echo -e "${BLUE}Installing Playwright dependencies...${NC}"
    if ! npx playwright install --with-deps; then
        echo -e "${RED}ERROR: Playwright installation failed in $(pwd)${NC}"
        popd > /dev/null
        return 1
    fi
    
    if [ -f ".env.server.headless" ]; then
        echo -e "${BLUE}Copying server environment configuration...${NC}"
        cp .env.server.headless .env.server
    fi
    
    if [ -f ".env.client.headless" ]; then
        echo -e "${BLUE}Copying client environment configuration...${NC}"
        cp .env.client.headless .env.client
    fi
    
    local app_test_failed=0
    
    # Run tests for each mode
    for mode in "$@"; do
        if ! run_tests_for_mode "$app_dir" "$mode"; then
            app_test_failed=1
        fi
    done
    
    popd > /dev/null
    
    local app_end_time=$(date +%s)
    local app_elapsed=$((app_end_time - app_start_time))
    local minutes=$((app_elapsed / 60))
    local seconds=$((app_elapsed % 60))
    
    echo -e "\n${YELLOW}Completed in: ${minutes}m ${seconds}s${NC}"
    
    return $app_test_failed
}

# Process test results for an app
process_test_results() {
    local app_dir="$1"
    local test_modes="$2"
    local status="$3"
    
    total_apps_tested=$((total_apps_tested + 1))
    
    if [ $status -eq 0 ]; then
        passed_apps_count=$((passed_apps_count + 1))
        echo -e "${GREEN}✓ OVERALL PASS: ${app_dir}${NC}"
    else
        failed_apps_count=$((failed_apps_count + 1))
        failed_apps+=("$app_dir ($test_modes)")
        echo -e "${RED}✗ OVERALL FAIL: ${app_dir}${NC}"
    fi
}

# Start time for the entire test run
start_time_all=$(date +%s)

echo -e "${YELLOW}${BOLD}Running Playwright tests for all specified Wasp applications...${NC}\n"

# Test applications specified for 'dev' mode only
for app_dir in "${apps_dev_only[@]}"; do
    run_tests_for_app "$app_dir" "dev"
    process_test_results "$app_dir" "dev mode" $?
done

# Test applications specified for both 'dev' and 'build' modes
for app_dir in "${apps_full_test[@]}"; do
    run_tests_for_app "$app_dir" "dev" "build"
    process_test_results "$app_dir" "dev+build modes" $?
done

# Calculate total elapsed time
end_time_all=$(date +%s)
elapsed_all=$((end_time_all - start_time_all))
minutes=$((elapsed_all / 60))
seconds=$((elapsed_all % 60))

echo -e "\n${YELLOW}${BOLD}-------------------- Summary --------------------${NC}"
echo -e "${YELLOW}All Wasp example application tests complete in ${minutes}m ${seconds}s.${NC}"
echo -e "Final Summary:"
echo -e "  Total applications tested: ${total_apps_tested}"

if [ $passed_apps_count -eq $total_apps_tested ]; then
    echo -e "  ${GREEN}All applications passed! (${passed_apps_count}/${total_apps_tested})${NC}"
else
    echo -e "  ${GREEN}Passed: ${passed_apps_count}/${total_apps_tested}${NC}"
    echo -e "  ${RED}Failed: ${failed_apps_count}/${total_apps_tested}${NC}"
    
    echo -e "\n${RED}Failed applications:${NC}"
    for failed_app in "${failed_apps[@]}"; do
        echo -e "  ${RED}• ${failed_app}${NC}"
    done
fi

if [ $failed_apps_count -ne 0 ]; then
    exit 1
fi
exit 0

