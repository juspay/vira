import { test, expect } from '@playwright/test';
import { ViraTestServer } from './utils';

test.describe('Vira Basic Integration Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Reset database and wait for server to be ready
    ViraTestServer.resetDatabase();
    await ViraTestServer.waitForServer(page);
    await ViraTestServer.navigateToHome(page);
  });

  test('should load Vira homepage', async ({ page }) => {
    await expect(page).toHaveTitle(/Vira/i);
    
    // Check that basic UI elements are present
    await expect(page.locator('body')).toBeVisible();
  });

  test('should add a new repository', async ({ page }) => {
    const repoName = 'test-repo';
    const cloneUrl = 'https://github.com/juspay/omnix.git';

    // Add repository through UI
    await ViraTestServer.addRepository(page, repoName, cloneUrl);

    // Verify repository appears in the list
    await expect(page.locator(`text=${repoName}`)).toBeVisible();
  });

  test('should build a repository', async ({ page }) => {
    const repoName = 'build-test-repo';
    const cloneUrl = 'https://github.com/juspay/omnix.git';

    // First add the repository
    await ViraTestServer.addRepository(page, repoName, cloneUrl);

    // Then trigger a build
    await ViraTestServer.triggerBuild(page, repoName);

    // Wait for build logs or status to appear
    await expect(page.locator('.build-log, .build-status, [data-testid="build-logs"]')).toBeVisible({ timeout: 20000 });

    // Optional: Wait for build completion (this might take a while)
    // const result = await ViraTestServer.waitForBuildCompletion(page, 120000);
    // expect(['success', 'failure']).toContain(result);
  });

  test('should navigate between pages', async ({ page }) => {
    // Test basic navigation
    await ViraTestServer.navigateToHome(page);
    
    // Check if there are any navigation links
    const links = await page.locator('a[href]').count();
    expect(links).toBeGreaterThan(0);
  });
});