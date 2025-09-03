import { execSync } from 'child_process';
import { Page } from '@playwright/test';

/**
 * Server lifecycle utilities for Vira tests
 */
export class ViraTestServer {
  private static port = 5005;

  /**
   * Reset the Vira database for clean test state
   */
  static resetDatabase(): void {
    try {
      execSync('cd .. && just resetdb', { 
        stdio: 'pipe',
        timeout: 10000 
      });
    } catch (error) {
      console.warn('Failed to reset database:', error);
    }
  }

  /**
   * Wait for Vira server to be healthy
   */
  static async waitForServer(page: Page, timeoutMs = 30000): Promise<void> {
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeoutMs) {
      try {
        const response = await page.goto('https://127.0.0.1:5006');
        if (response?.ok()) {
          console.log('✓ Vira server is ready');
          return;
        }
      } catch (error) {
        // Server not ready yet, continue waiting
      }
      
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
    
    throw new Error(`Vira server did not become ready within ${timeoutMs}ms`);
  }

  /**
   * Navigate to Vira home page and wait for it to load
   */
  static async navigateToHome(page: Page): Promise<void> {
    await page.goto('/');
    // Wait for domcontentloaded instead of networkidle since Vira has streaming connections
    await page.waitForLoadState('domcontentloaded');
    // Wait a bit more for any immediate content to render
    await page.waitForTimeout(1000);
  }

  /**
   * Add a new repository through the UI
   */
  static async addRepository(page: Page, name: string, cloneUrl: string): Promise<void> {
    try {
      // Look for add repository form elements - adjust selectors based on actual Vira UI
      const nameInput = page.locator('input[name="name"], input[placeholder*="name"], input[id*="name"]').first();
      const urlInput = page.locator('input[name="cloneUrl"], input[name="url"], input[placeholder*="url"], input[placeholder*="repository"]').first();
      const submitButton = page.locator('button[type="submit"], button:has-text("Add"), button:has-text("Create")').first();
      
      // Fill the form
      await nameInput.fill(name);
      await urlInput.fill(cloneUrl);
      await submitButton.click();
      
      // Wait for success indication or repository to appear
      await page.waitForTimeout(2000);
    } catch (error) {
      console.log('Add repository form not found or failed:', error);
      throw new Error(`Could not add repository: ${error}`);
    }
  }

  /**
   * Trigger a build for a repository
   */
  static async triggerBuild(page: Page, repoName: string): Promise<void> {
    // Navigate to repository page
    await page.click(`a:has-text("${repoName}")`);
    
    // Click build button
    await page.click('button:has-text("Build")');
    
    // Wait for build to start (look for some indication like logs or status change)
    await page.waitForSelector('.build-log, .build-status, [data-testid="build-logs"]', { timeout: 15000 });
  }

  /**
   * Wait for build completion (either success or failure)
   */
  static async waitForBuildCompletion(page: Page, timeoutMs = 60000): Promise<'success' | 'failure'> {
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeoutMs) {
      const buildStatus = await page.textContent('.build-status');
      
      if (buildStatus?.toLowerCase().includes('success') || 
          buildStatus?.toLowerCase().includes('completed')) {
        return 'success';
      }
      
      if (buildStatus?.toLowerCase().includes('failed') || 
          buildStatus?.toLowerCase().includes('error')) {
        return 'failure';
      }
      
      await new Promise(resolve => setTimeout(resolve, 2000));
    }
    
    throw new Error(`Build did not complete within ${timeoutMs}ms`);
  }
}