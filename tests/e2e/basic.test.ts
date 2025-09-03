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

  test('should show page content and have basic UI elements', async ({ page }) => {
    // Check for basic page structure
    await expect(page.locator('body')).toBeVisible();
    
    // Check that the page has loaded some content (not just a blank page)
    const bodyText = await page.locator('body').textContent();
    expect(bodyText).toBeTruthy();
    expect(bodyText!.length).toBeGreaterThan(10);
  });

  test('should have accessible page elements', async ({ page }) => {
    // Check for common HTML elements that indicate the page loaded properly
    const hasHeadings = await page.locator('h1, h2, h3').count();
    const hasButtons = await page.locator('button').count();
    const hasInputs = await page.locator('input').count();
    
    // At least some interactive elements should be present
    expect(hasHeadings + hasButtons + hasInputs).toBeGreaterThan(0);
  });

  test('should navigate between pages', async ({ page }) => {
    // Test basic navigation
    await ViraTestServer.navigateToHome(page);
    
    // Check if there are any navigation links
    const links = await page.locator('a[href]').count();
    expect(links).toBeGreaterThan(0);
  });
});