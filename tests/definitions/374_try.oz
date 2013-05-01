% try-finally expressions
% Exception raised
{Show try raise stopHere end b
      catch
         stopHere then
         'stopped'
      finally
         {Show 'and finished'}
      end}

% Success
{Show try allOk
      catch
         stopHere then
         'stopped'
      finally
         {Show 'and finished'}
      end}
% Success with integer value
{Show try 3
      catch
         stopHere then
         'stopped'
      finally
         {Show 'and finished'}
      end}

